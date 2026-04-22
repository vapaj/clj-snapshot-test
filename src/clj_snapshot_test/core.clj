(ns clj-snapshot-test.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn snapshot-path [test-name]
  (let [test-var (first clojure.test/*testing-vars*)
        test-file (some-> test-var meta :file)
        test-file-stem (if (str/blank? test-file)
                         "unknown_test"
                         (-> test-file
                             io/file
                             .getName
                             (str/replace #"\.clj[cs]?$" "")))]
    (str "test/snapshots/" test-file-stem "/" test-name ".snap")))

(defn create-snapshot-file! [test-name content]
  (let [snap-file (snapshot-path test-name)]
    (io/make-parents snap-file)
    (spit snap-file content)))

(defn colorize [s ansi-code]
  (str "\u001b[" ansi-code "m" s "\u001b[0m"))

(defn colorize-diff-line [line]
  (cond
    (str/starts-with? line "+ ") (colorize line "32")
    (str/starts-with? line "- ") (colorize line "31")
    :else line))

(defn mark-diff [file-content test-result-content]
  (let [x (into file-content (repeat (- (count test-result-content) (count file-content)) nil))
        y (into test-result-content (repeat (- (count file-content) (count test-result-content)) nil))
        zipped (mapv vector x y)]
    (reduce (fn [acc [old-row new-row]]
              (cond
                (nil? old-row) (conj acc (colorize-diff-line (str "+ " new-row)))
                (nil? new-row) (conj acc (colorize-diff-line (str "- " old-row)))
                (not= old-row new-row) (conj acc (colorize-diff-line (str "- " old-row))
                                             (colorize-diff-line (str "+ " new-row)))
                :else (conj acc (str "  " new-row))))
            []
            zipped)))

(defn update-snapshots-enabled? []
  (contains? #{"true" "1" "yes" "y"}
             (some-> (System/getProperty "aoc.updateSnapshots")
                     (str/lower-case))))

(defn matches-snapshot [test-name content]
  (let [snap-file (snapshot-path test-name)
        content-lines (str/split-lines content)
        snapshot-exists? (.exists (io/file snap-file))]
    (if snapshot-exists?
      (let [file-lines (str/split-lines (slurp snap-file))
            diff (mark-diff file-lines content-lines)
            same? (= file-lines content-lines)]
        (cond
          same?
          {:pass? true
           :snapshot-file snap-file
           :expected file-lines
           :actual content-lines
           :diff diff}

          (update-snapshots-enabled?)
          (do
            (create-snapshot-file! test-name content)
            {:pass? true
             :updated? true
             :snapshot-file snap-file
             :expected file-lines
             :actual content-lines
             :diff diff})

          :else
          {:pass? false
           :snapshot-file snap-file
           :expected file-lines
           :actual content-lines
           :diff diff}))
      (do
        (create-snapshot-file! test-name content)
        {:pass? true
         :created? true
         :snapshot-file snap-file
         :expected content-lines
         :actual content-lines
         :diff []}))))

(defmethod clojure.test/assert-expr 'matches-snapshot [msg form]
  (let [[_ test-name content] form]
    `(let [result# (matches-snapshot ~test-name ~content)]
       (if (:pass? result#)
         (clojure.test/do-report {:type :pass
                                  :message ~msg
                                  :expected '~form
                                  :actual (cond
                                            (:updated? result#) (str "updated " (:snapshot-file result#))
                                            (:created? result#) (str "created " (:snapshot-file result#))
                                            :else (str "matched " (:snapshot-file result#)))})
         (do
           (clojure.test/do-report {:type :fail
                                    :message (or ~msg
                                                 (str "Snapshot mismatch in " (:snapshot-file result#)))
                                    :expected (:expected result#)
                                    :actual (:actual result#)})
           (clojure.test/with-test-out
             (println "\nSnapshot mismatch in" (:snapshot-file result#))
             (doseq [line# (:diff result#)]
               (println line#)))))
       (:pass? result#))))
