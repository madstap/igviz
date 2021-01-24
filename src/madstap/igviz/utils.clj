(ns madstap.igviz.utils
  (:require
   [clojure.java.browse :as java.browse]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.io File)))

(defn add-extension [s extension]
  (str s (when-not (str/ends-with? s extension) extension)))

(defn extension [format]
  (str "." (name format)))

(defn temp-file [format]
  (File/createTempFile (str (gensym "igviz-")) (extension format)))

(defn new-file [path format]
  (if (nil? path)
    (temp-file format)
    (File. ^String (add-extension path (extension format)))))

(defn save-bytes! [file bytes]
  (with-open [output (io/output-stream file)]
    (io/copy bytes output)))

(defn save-graph! [graph path format]
  (doto (new-file path format) (save-bytes! graph)))

(defn open! [^File file]
  (future (java.browse/browse-url (.toURL file))))
