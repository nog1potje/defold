(ns dynamo.file
  "Functions to help developers load and save files"
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [internal.java :as j])
  (:import [com.google.protobuf TextFormat]))

(defn- new-builder
  [class]
  (j/invoke-no-arg-class-method class "newBuilder"))

(defonce ^:private loaders (atom {}))

(defn register-loader
  [filetype loader]
  (swap! loaders assoc filetype loader))

(defn loader-for
  [filename]
  (let [lfs @loaders]
    (or
      (some (fn [[filetype lf]] (when (.endsWith filename filetype) lf)) lfs)
      (fn [_ _] (throw (ex-info (str "No loader has been registered that can handle " filename) {}))))))

(defn protocol-buffer-loader
  [^java.lang.Class class f]
  (fn [nm input-reader]
    (let [builder (new-builder class)]
      (TextFormat/merge input-reader builder)
      (f nm (.build builder)))))

(defmulti message->node
  (fn [message container container-target desired-output] (class message)))

(defn load

  [filename]
  ((loader-for filename) filename (io/reader filename)))


(doseq [[v doc]
       {#'new-builder
        "Dynamically construct a protocol buffer builder, given a class as a variable."

        #'register-loader
        "Associate a filetype (extension) with a loader function. The given loader will be
used any time a file with that type is opened."

        #'loader-for
        "Locate a loading function that knows how to work on the given file.

If no suitable function has been registered, this returns a function
that throws an exception."

        #'protocol-buffer-loader
          "Create a new loader that knows how to read protocol buffer files in text format.

class is the Java class generated by the protoc compiler. It will probably be an inner class
of some top-level name. Instead of a '.' for the inner class name separator, use a '$'.

For example, the inner class called AtlasProto.Atlas in Java becomes AtlasProto$Atlas.

f is a function to call with the deserialised protocol buffer message. f must take two arguments, the
resource name and the immutable protocol buffer itself."


        #'message->node
          "This is an extensible function that you implement to help load a specific file
type. Most of the time, these will be created for you by the
dynamo.file.protobuf/protocol-buffer-converter macro.

Create an implementation by adding something like this to your namespace:

(defmethod message->node _message-classname_
  [_message-instance_ container container-target desired-output]
  (,,,) ;; implementation
)

You'll replace _message-classname_ with the Java class that matches the message
type to convert. The _message-instance_ argument will contain an instance of the
class specified in _message-classname_.

If the message instance contains other messages (as in the case of protocol buffers,
for example) then you should call message->node recursively with the same resource-name
and the child message.

Given a resource name and message describing the resource,
create (and return?) a list of nodes."

          #'load
        "Load a file. This looks up a suitable loader based on the filename. Loaders must be
registered via register-loader before they can be used.

This will invoke the loader function with the filename and a reader to supply the file contents."
        }]
  (alter-meta! v assoc :doc doc))


