#include <jni.h>
#include <stdlib.h>
#include <unistd.h>
#include <dlib/array.h>
#include <dlib/log.h>
#include <dlib/dstrings.h>
#include <dlib/json.h>
#include <script/script.h>
#include <extension/extension.h>
#include <android_native_app_glue.h>
#include "push_utils.h"

#define LIB_NAME "push"

extern struct android_app* g_AndroidApp;

struct Push;

#define CMD_REGISTRATION_RESULT (0)
#define CMD_PUSH_MESSAGE_RESULT (1)

struct Command
{
    Command()
    {
        memset(this, 0, sizeof(*this));
    }
    uint32_t m_Command;
    int32_t  m_ResponseCode;
    void*    m_Data1;
    void*    m_Data2;
};

static JNIEnv* Attach()
{
    JNIEnv* env;
    g_AndroidApp->activity->vm->AttachCurrentThread(&env, NULL);
    return env;
}

static void Detach()
{
    g_AndroidApp->activity->vm->DetachCurrentThread();
}

struct PushListener
{
    PushListener()
    {
        m_L = 0;
        m_Callback = LUA_NOREF;
        m_Self = LUA_NOREF;
    }
    lua_State* m_L;
    int        m_Callback;
    int        m_Self;
};

struct Push
{
    Push()
    {
        memset(this, 0, sizeof(*this));
        m_Callback = LUA_NOREF;
        m_Self = LUA_NOREF;
        m_Listener.m_Callback = LUA_NOREF;
        m_Listener.m_Self = LUA_NOREF;
        m_ScheduleLastID = 0;
    }
    int                  m_Callback;
    int                  m_Self;
    lua_State*           m_L;
    PushListener         m_Listener;

    jobject              m_Push;
    jobject              m_PushJNI;
    jmethodID            m_Start;
    jmethodID            m_Stop;
    jmethodID            m_Register;
    jmethodID            m_Schedule;
    jmethodID            m_Cancel;
    int                  m_Pipefd[2];

    int                  m_ScheduleLastID;
};

Push g_Push;

static void VerifyCallback(lua_State* L)
{
    if (g_Push.m_Callback != LUA_NOREF) {
        dmLogError("Unexpected callback set");
        luaL_unref(L, LUA_REGISTRYINDEX, g_Push.m_Callback);
        luaL_unref(L, LUA_REGISTRYINDEX, g_Push.m_Self);
        g_Push.m_Callback = LUA_NOREF;
        g_Push.m_Self = LUA_NOREF;
        g_Push.m_L = 0;
    }
}

int Push_Register(lua_State* L)
{
    int top = lua_gettop(L);
    VerifyCallback(L);

    // NOTE: We ignore argument one. Only for iOS
    luaL_checktype(L, 2, LUA_TFUNCTION);
    lua_pushvalue(L, 2);
    g_Push.m_Callback = luaL_ref(L, LUA_REGISTRYINDEX);
    dmScript::GetInstance(L);
    g_Push.m_Self = luaL_ref(L, LUA_REGISTRYINDEX);
    g_Push.m_L = dmScript::GetMainThread(L);

    JNIEnv* env = Attach();
    env->CallVoidMethod(g_Push.m_Push, g_Push.m_Register, g_AndroidApp->activity->clazz);
    Detach();

    assert(top == lua_gettop(L));
    return 0;
}

int Push_SetListener(lua_State* L)
{
    Push* push = &g_Push;
    luaL_checktype(L, 1, LUA_TFUNCTION);
    lua_pushvalue(L, 1);
    int cb = luaL_ref(L, LUA_REGISTRYINDEX);

    if (push->m_Listener.m_Callback != LUA_NOREF) {
        luaL_unref(push->m_Listener.m_L, LUA_REGISTRYINDEX, push->m_Listener.m_Callback);
        luaL_unref(push->m_Listener.m_L, LUA_REGISTRYINDEX, push->m_Listener.m_Self);
    }

    push->m_Listener.m_L = dmScript::GetMainThread(L);
    push->m_Listener.m_Callback = cb;

    dmScript::GetInstance(L);
    push->m_Listener.m_Self = luaL_ref(L, LUA_REGISTRYINDEX);

    return 0;
}

int Push_Schedule(lua_State* L)
{
    int top = lua_gettop(L);
    int seconds = luaL_checkinteger(L, 1);
    if (seconds < 0)
    {
        lua_pushnil(L);
        lua_pushstring(L, "invalid seconds argument");
        return 2;
    }

    const char* title = luaL_checkstring(L, 2);
    const char* message = luaL_checkstring(L, 3);

    // param: userdata
    const char* userdata = 0;
    if (top > 3) {
        userdata = luaL_checkstring(L, 4);
    }

    // param: notification_settings
    int priority = 3;
    const char* group = 0;
    // char* icon = 0;
    // char* sound = 0;
    if (top > 4) {
        luaL_checktype(L, 5, LUA_TTABLE);

        // priority
        lua_pushstring(L, "priority");
        lua_gettable(L, 5);
        if (lua_isnumber(L, -1)) {
            priority = lua_tointeger(L, -1);

            if (priority < 0) {
                priority = 0;
            } else if (priority > 5) {
                priority = 5;
            }
        }
        lua_pop(L, 1);

        // group
        lua_pushstring(L, "group");
        lua_gettable(L, 5);
        if (lua_isstring(L, -1)) {
            group = lua_tostring(L, -1);
        }
        lua_pop(L, 1);

        /*
        // icon
        There is now way of automatically bundle files inside the .app folder (i.e. skipping
        archiving them inside the .darc), but to have custom notification sounds they need to
        be accessable from the .app folder.

        lua_pushstring(L, "icon");
        lua_gettable(L, 5);
        if (lua_isstring(L, -1)) {
            icon = lua_tostring(L, -1);
        }
        lua_pop(L, 1);

        // sound
        lua_pushstring(L, "sound");
        lua_gettable(L, 5);
        if (lua_isstring(L, -1)) {
            notification.soundName = [NSString stringWithUTF8String:lua_tostring(L, -1)];
        }
        lua_pop(L, 1);
        */
    }

    JNIEnv* env = Attach();
    jstring jtitle    = env->NewStringUTF(title);
    jstring jmessage  = env->NewStringUTF(message);
    jstring juserdata = env->NewStringUTF(userdata);
    jstring jgroup    = env->NewStringUTF(group);
    env->CallVoidMethod(g_Push.m_Push, g_Push.m_Schedule, g_AndroidApp->activity->clazz, g_Push.m_ScheduleLastID, seconds, jtitle, jmessage, juserdata, jgroup, priority);
    env->DeleteLocalRef(jgroup);
    env->DeleteLocalRef(juserdata);
    env->DeleteLocalRef(jmessage);
    env->DeleteLocalRef(jtitle);
    Detach();

    assert(top == lua_gettop(L));

    lua_pushnumber(L, g_Push.m_ScheduleLastID++);
    return 1;

}

int Push_Cancel(lua_State* L)
{
    int cancel_id = luaL_checkinteger(L, 1);
    JNIEnv* env = Attach();
    env->CallVoidMethod(g_Push.m_Push, g_Push.m_Cancel, g_AndroidApp->activity->clazz, cancel_id);
    Detach();

    return 0;
}

static const luaL_reg Push_methods[] =
{
    {"register", Push_Register},
    {"set_listener", Push_SetListener},

    {"schedule", Push_Schedule},
    {"cancel", Push_Cancel},

    {0, 0}
};


#ifdef __cplusplus
extern "C" {
#endif

static void PushError(lua_State*L, const char* error)
{
    // Could be extended with error codes etc
    if (error != 0) {
        lua_newtable(L);
        lua_pushstring(L, "error");
        lua_pushstring(L, error);
        lua_rawset(L, -3);
    } else {
        lua_pushnil(L);
    }
}

JNIEXPORT void JNICALL Java_com_defold_push_PushJNI_onRegistration(JNIEnv* env, jobject, jstring regId, jstring errorMessage)
{
    const char* ri = 0;
    const char* em = 0;

    if (regId)
    {
        ri = env->GetStringUTFChars(regId, 0);
    }
    if (errorMessage)
    {
        em = env->GetStringUTFChars(errorMessage, 0);
    }

    Command cmd;
    cmd.m_Command = CMD_REGISTRATION_RESULT;
    if (ri) {
        cmd.m_Data1 = strdup(ri);
        env->ReleaseStringUTFChars(regId, ri);
    }
    if (em) {
        cmd.m_Data2 = strdup(em);
        env->ReleaseStringUTFChars(errorMessage, em);
    }
    if (write(g_Push.m_Pipefd[1], &cmd, sizeof(cmd)) != sizeof(cmd)) {
        dmLogFatal("Failed to write command");
    }
}


JNIEXPORT void JNICALL Java_com_defold_push_PushJNI_onMessage(JNIEnv* env, jobject, jstring json)
{
    const char* j = 0;

    if (json)
    {
        j = env->GetStringUTFChars(json, 0);
    }

    Command cmd;
    cmd.m_Command = CMD_PUSH_MESSAGE_RESULT;
    cmd.m_Data1 = strdup(j);
    if (write(g_Push.m_Pipefd[1], &cmd, sizeof(cmd)) != sizeof(cmd)) {
        dmLogFatal("Failed to write command");
    }
    if (j)
    {
        env->ReleaseStringUTFChars(json, j);
    }
}

#ifdef __cplusplus
}
#endif

void HandleRegistrationResult(const Command* cmd)
{
    if (g_Push.m_Callback == LUA_NOREF) {
        dmLogError("No callback set");
        return;
    }

    lua_State* L = g_Push.m_L;
    int top = lua_gettop(L);

    lua_rawgeti(L, LUA_REGISTRYINDEX, g_Push.m_Callback);

    // Setup self
    lua_rawgeti(L, LUA_REGISTRYINDEX, g_Push.m_Self);
    lua_pushvalue(L, -1);
    dmScript::SetInstance(L);

    if (!dmScript::IsInstanceValid(L))
    {
        dmLogError("Could not run push callback because the instance has been deleted.");
        lua_pop(L, 2);
        assert(top == lua_gettop(L));
        return;
    }

    if (cmd->m_Data1) {
        lua_pushstring(L, (const char*) cmd->m_Data1);
        lua_pushnil(L);
    } else {
        lua_pushnil(L);
        PushError(L, (const char*) cmd->m_Data2);
        dmLogError("GCM error %s", (const char*) cmd->m_Data2);
    }

    dmScript::PCall(L, 3, LUA_MULTRET);

    luaL_unref(L, LUA_REGISTRYINDEX, g_Push.m_Callback);
    luaL_unref(L, LUA_REGISTRYINDEX, g_Push.m_Self);
    g_Push.m_Callback = LUA_NOREF;
    g_Push.m_Self = LUA_NOREF;

    assert(top == lua_gettop(L));
}

void HandlePushMessageResult(const Command* cmd)
{
    if (g_Push.m_Listener.m_Callback == LUA_NOREF) {
        dmLogError("No callback set");
        return;
    }

    lua_State* L = g_Push.m_Listener.m_L;
    int top = lua_gettop(L);

    lua_rawgeti(L, LUA_REGISTRYINDEX, g_Push.m_Listener.m_Callback);

    // Setup self
    lua_rawgeti(L, LUA_REGISTRYINDEX, g_Push.m_Listener.m_Self);
    lua_pushvalue(L, -1);
    dmScript::SetInstance(L);

    if (!dmScript::IsInstanceValid(L))
    {
        dmLogError("Could not run push callback because the instance has been deleted.");
        lua_pop(L, 2);
        assert(top == lua_gettop(L));
        return;
    }

    dmJson::Document doc;
    dmJson::Result r = dmJson::Parse((const char*) cmd->m_Data1, &doc);
    if (r == dmJson::RESULT_OK && doc.m_NodeCount > 0) {
        JsonToLua(L, &doc, 0);
        dmScript::PCall(L, 2, LUA_MULTRET);
    } else {
        dmLogError("Failed to parse push response (%d)", r);
    }
    dmJson::Free(&doc);

    assert(top == lua_gettop(L));
}

static int LooperCallback(int fd, int events, void* data)
{
    Push* fb = (Push*)data;
    (void)fb;
    Command cmd;
    if (read(g_Push.m_Pipefd[0], &cmd, sizeof(cmd)) == sizeof(cmd)) {
        switch (cmd.m_Command)
        {
        case CMD_REGISTRATION_RESULT:
            HandleRegistrationResult(&cmd);
            break;
        case CMD_PUSH_MESSAGE_RESULT:
            HandlePushMessageResult(&cmd);
            break;

        default:
            assert(false);
        }

        if (cmd.m_Data1) {
            free(cmd.m_Data1);
        }
        if (cmd.m_Data2) {
            free(cmd.m_Data2);
        }
    }
    else {
        dmLogFatal("read error in looper callback");
    }
    return 1;
}

dmExtension::Result AppInitializePush(dmExtension::AppParams* params)
{
    int result = pipe(g_Push.m_Pipefd);
    if (result != 0) {
        dmLogFatal("Could not open pipe for communication: %d", result);
        return dmExtension::RESULT_INIT_ERROR;
    }

    result = ALooper_addFd(g_AndroidApp->looper, g_Push.m_Pipefd[0], ALOOPER_POLL_CALLBACK, ALOOPER_EVENT_INPUT, LooperCallback, &g_Push);
    if (result != 1) {
        dmLogFatal("Could not add file descriptor to looper: %d", result);
        close(g_Push.m_Pipefd[0]);
        close(g_Push.m_Pipefd[1]);
        return dmExtension::RESULT_INIT_ERROR;
    }

    JNIEnv* env = Attach();

    jclass activity_class = env->FindClass("android/app/NativeActivity");
    jmethodID get_class_loader = env->GetMethodID(activity_class,"getClassLoader", "()Ljava/lang/ClassLoader;");
    jobject cls = env->CallObjectMethod(g_AndroidApp->activity->clazz, get_class_loader);
    jclass class_loader = env->FindClass("java/lang/ClassLoader");
    jmethodID find_class = env->GetMethodID(class_loader, "loadClass", "(Ljava/lang/String;)Ljava/lang/Class;");

    jstring str_class_name = env->NewStringUTF("com.defold.push.Push");
    jclass push_class = (jclass)env->CallObjectMethod(cls, find_class, str_class_name);
    env->DeleteLocalRef(str_class_name);

    str_class_name = env->NewStringUTF("com.defold.push.PushJNI");
    jclass push_jni_class = (jclass)env->CallObjectMethod(cls, find_class, str_class_name);
    env->DeleteLocalRef(str_class_name);

    g_Push.m_Start = env->GetMethodID(push_class, "start", "(Landroid/app/Activity;Lcom/defold/push/IPushListener;Ljava/lang/String;)V");
    g_Push.m_Stop = env->GetMethodID(push_class, "stop", "()V");
    g_Push.m_Register = env->GetMethodID(push_class, "register", "(Landroid/app/Activity;)V");
    g_Push.m_Schedule = env->GetMethodID(push_class, "scheduleNotification", "(Landroid/app/Activity;IILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)V");
    g_Push.m_Cancel = env->GetMethodID(push_class, "cancelNotification", "(Landroid/app/Activity;I)V");

    jmethodID get_instance_method = env->GetStaticMethodID(push_class, "getInstance", "()Lcom/defold/push/Push;");
    g_Push.m_Push = env->NewGlobalRef(env->CallStaticObjectMethod(push_class, get_instance_method));

    jmethodID jni_constructor = env->GetMethodID(push_jni_class, "<init>", "()V");
    g_Push.m_PushJNI = env->NewGlobalRef(env->NewObject(push_jni_class, jni_constructor));

    const char* sender_id = dmConfigFile::GetString(params->m_ConfigFile, "android.gcm_sender_id", "");
    jstring sender_id_string = env->NewStringUTF(sender_id);
    env->CallVoidMethod(g_Push.m_Push, g_Push.m_Start, g_AndroidApp->activity->clazz, g_Push.m_PushJNI, sender_id_string);
    env->DeleteLocalRef(sender_id_string);

    Detach();

    return dmExtension::RESULT_OK;
}

dmExtension::Result AppFinalizePush(dmExtension::AppParams* params)
{
    JNIEnv* env = Attach();
    env->CallVoidMethod(g_Push.m_Push, g_Push.m_Stop);
    env->DeleteGlobalRef(g_Push.m_Push);
    env->DeleteGlobalRef(g_Push.m_PushJNI);
    Detach();
    g_Push.m_Push = NULL;
    g_Push.m_PushJNI = NULL;
    g_Push.m_L = 0;
    g_Push.m_Callback = LUA_NOREF;
    g_Push.m_Self = LUA_NOREF;

    int result = ALooper_removeFd(g_AndroidApp->looper, g_Push.m_Pipefd[0]);
    if (result != 1) {
        dmLogFatal("Could not remove fd from looper: %d", result);
    }

    close(g_Push.m_Pipefd[0]);
    env = Attach();
    close(g_Push.m_Pipefd[1]);
    Detach();

    return dmExtension::RESULT_OK;
}

dmExtension::Result InitializePush(dmExtension::Params* params)
{
    lua_State*L = params->m_L;
    int top = lua_gettop(L);
    luaL_register(L, LIB_NAME, Push_methods);
    lua_pop(L, 1);
    assert(top == lua_gettop(L));
    return dmExtension::RESULT_OK;
}

dmExtension::Result FinalizePush(dmExtension::Params* params)
{
    if (params->m_L == g_Push.m_Listener.m_L && g_Push.m_Listener.m_Callback != LUA_NOREF) {
        luaL_unref(g_Push.m_Listener.m_L, LUA_REGISTRYINDEX, g_Push.m_Listener.m_Callback);
        luaL_unref(g_Push.m_Listener.m_L, LUA_REGISTRYINDEX, g_Push.m_Listener.m_Self);
        g_Push.m_Listener.m_L = 0;
        g_Push.m_Listener.m_Callback = LUA_NOREF;
        g_Push.m_Listener.m_Self = LUA_NOREF;
    }

    return dmExtension::RESULT_OK;
}

DM_DECLARE_EXTENSION(PushExt, "Push", AppInitializePush, AppFinalizePush, InitializePush, FinalizePush)
