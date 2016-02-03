(ns haiku.core
    (:require [clojure.java.io :as io])
    (:gen-class))

(defrecord SpeechParts [adjectives adverbs nouns verbs prepositions])

(defn speechtxt-to-seq
    "Given a part of speech, and a number of syllables, reads the
     corresponding txt file into a seq"
    [part, syls]
    (let [path (str "haiku/speech/" part "s/" syls "syllable" part "s.txt")]
        (vec (line-seq (io/reader (io/resource path))))))

(defn read-speech-part
    "Reads a certain part of speech from resources
     Example: (read-speech-part 'noun') or (read-speech-part 'verb') "
    [part]
    (map (partial speechtxt-to-seq part) [1,2,3,4]))

(defn read-parts-of-speech []
    (let [parts ["adjective" "adverb" "noun" "verb" "preposition"]]
        (apply ->SpeechParts (map read-speech-part parts))))

(defn structtxt-to-seq [syllables]
    (let [path (str "haiku/structure/" syllables "syllablestructures.txt")]
        (vec (line-seq (io/reader (io/resource path))))))

(defn read-structure [syllables]
    (->> syllables structtxt-to-seq rand-nth))

(defn part-from-structure
    "Takes a word with a number and a part of speech, (ex: 1verb)
     and returns a part of speech from parts SpeechParts passed to it"
     [word, parts]
     (let [syls (read-string (subs word 0 1)), part-str (subs word 1)]
         (-> (get parts (keyword part-str)) (nth (- syls 1)) rand-nth)))

(defn parse-structure-word
    "Takes a word that is from a structure, it replaces it with a part
     of speech if it starts with a ~"
     [parts, word]
     (if (= \~ (first word))
        (part-from-structure (subs word 1), parts)
        word))

(defn parse-structure-line
    "Takes a structure-line line which is a seq of words, some with ~ in front
     which indicate a part of speech"
    [line, parts]
    (let [line-with-replaces (map (partial parse-structure-word parts) line)]
        (clojure.string/join " " line-with-replaces)))

(defprotocol Poem
    (create [lines])
    (write-line [syllables]))

(defrecord Haiku [title]
Poem
(create [lines])
(write-line [hai]
    (let [structs (map read-structure [5,7,5]),
          speech-parts (read-parts-of-speech)]
          ; map parse structure-line to the split of the structs strings
          (map (fn [line]
                    (-> line (clojure.string/split #" ") (parse-structure-line speech-parts)))
                structs))))

(defn -main
    "I don't do a whole lot ... yet."
    [& args]
        (let [haiku (Haiku. "Random Poetry")]
            (println (nth (write-line haiku) 0))
            (println (nth (write-line haiku) 1))
            (println (nth (write-line haiku) 2))
            (println " ")
            ))
