// Copyright 2020-2024 The Defold Foundation
// Copyright 2014-2020 King
// Copyright 2009-2014 Ragnar Svensson, Christian Murray
// Licensed under the Defold License version 1.0 (the "License"); you may not use
// this file except in compliance with the License.
//
// You may obtain a copy of the License, together with FAQs at
// https://www.defold.com/license
//
// Unless required by applicable law or agreed to in writing, software distributed
// under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

#ifndef DMSDK_RESOURCE_HPP
#define DMSDK_RESOURCE_HPP

/*# Resource
 *
 * Functions for managing resource types.
 *
 * @document
 * @name Resource
 * @namespace dmResource
 * @path engine/resource/src/dmsdk/resource/resource.hpp
 */

#include <stdint.h>
#include <dmsdk/dlib/hash.h>

#include <dmsdk/resource/resource_params.h>
#include <dmsdk/resource/resource_desc.h>
#include <dmsdk/resource/resource_gen.hpp>

namespace dmResource
{
// Legacy
// We keep these structs in the public api until we've updated our extensions in the wild

    typedef HResourceFactory HFactory;

    /*#
     * Get a resource from factory
     * @name Get
     * @param factory [type: dmResource::HFactory] Factory handle
     * @param name [type: const char*] Resource name
     * @param resource [type: void**] Created resource
     * @return result [type: dmResource::Result]  RESULT_OK on success
     */
    Result Get(HFactory factory, const char* name, void** resource);

    /*#
     * Get a resource from factory
     * @name Get
     * @param factory [type: dmResource::HFactory] Factory handle
     * @param name [type: dmhash_t] Resource name
     * @param resource [type: void**] Created resource
     * @return result [type: dmResource::Result]  RESULT_OK on success
     */
    Result Get(HFactory factory, dmhash_t name, void** resource);


    /*#
     * Release resource
     * @name Release
     * @param factory [type: dmResource::HFactory] Factory handle
     * @param resource [type: void*] Resource pointer
     */
    void Release(HFactory factory, void* resource);

    /*#
     * Hint the preloader what to load before Create is called on the resource.
     * The resources are not guaranteed to be loaded before Create is called.
     * This function can be called from a worker thread.
     * @name PreloadHint
     * @param factory [type: dmResource::HResourcePreloadHintInfo] Preloader handle
     * @param name [type: const char*] Resource name
     * @return result [type: bool] if successfully invoking preloader.
     */
    bool PreloadHint(HResourcePreloadHintInfo preloader, const char *name);

    /*#
     * Returns the canonical path hash of a resource
     * @param factory [type: dmResource::HFactory] Factory handle
     * @param resource Resource
     * @param hash Returned hash
     * @return RESULT_OK on success
    */
    Result GetPath(HFactory factory, const void* resource, uint64_t* hash);

    /*#
     * Returns the canonical path hash of a resource
     * @typedef
     * @name FDecryptResource
     * @param buffer [type: void*] The input/output buffer
     * @param buffer_len [type: uint32_t] The size of the buffer (in bytes)
     * @return RESULT_OK on success
    */
    typedef Result (*FDecryptResource)(void* buffer, uint32_t buffer_len);

    /*#
     * Registers a custom resource decryption function
     * @name RegisterResourceDecryptionFunction
     * @param decrypt_resource [type: dmResource::FDecryptResource] The decryption function
    */
    void RegisterResourceDecryptionFunction(FDecryptResource decrypt_resource);
    /*#
     * Function called when a resource has been reloaded.
     * @param params Parameters
     * @see RESOURCE_FACTORY_FLAGS_RELOAD_SUPPORT
     * @see ResourceRegisterReloadedCallback
     */
    void RegisterResourceReloadedCallback(HFactory factory, FResourceReloadedCallback callback, void* user_data);

    /**
     * Remove a registered callback function, O(n).
     * @name ResourceUnregisterReloadedCallback
     * @param factory Handle of the factory from which the callback will be removed
     * @param callback Callback function to remove
     * @param user_data User data that was supplied when the callback was registered
     * @see RESOURCE_FACTORY_FLAGS_RELOAD_SUPPORT
     */
    void UnregisterResourceReloadedCallback(HFactory factory, FResourceReloadedCallback callback, void* user_data);


    /*#
     * Adds a file to the resource system
     * Any request for this path will go through any existing mounts first.
     * If you wish to provide file overrides, please use the LiveUpdate feature for that.
     * The file isn't persisted between sessions.
     *
     * @name AddFile
     * @param factory [type: dmResource::HFactory] Factory handle
     * @param path [type: const char*] The path of the resource
     * @param size [type: uint32_t] The size of the resource (in bytes)
     * @param resource [type: const void*] The resource payload
     * @return RESULT_OK on success.
     */
    Result AddFile(HFactory factory, const char* path, uint32_t size, const void* resource);

    /*#
     * Removes a previously registered file from the resource system
     * @name RemoveFile
     * @param factory [type: dmResource::HFactory] Factory handle
     * @param path [type: const char*] The path of the resource
     * @return RESULT_OK on success.
     */
    Result RemoveFile(HFactory factory, const char* path);
}

#endif // DMSDK_RESOURCE_HPP
