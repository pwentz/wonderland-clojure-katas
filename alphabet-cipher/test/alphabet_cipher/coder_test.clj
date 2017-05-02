(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(deftest test-format-keyword
  (testing "can cycle keyword to length of message"
    (is (= "vigilancevigilancevigilancevi"
           (format-keyword "vigilance" "meetmeontuesdayeveningatseven")))))

(deftest test-assemble-encoding
  (testing "can take two chars and return the intersecting char from chart"
    (is (= \h
           (assemble-encoding [\v \m])))))

(deftest test-encode
  (testing "can encode given a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))

(deftest test-assemble-decoding
  (testing "can take two chars and return the reverse-intersecting char"
    (is (= \m
           (assemble-decoding [\v \h])))))

(deftest test-decode
  (testing "can decode an cyrpted message given a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))

; (deftest test-decipher
;   (testing "can extract the secret keyword given an encrypted message and the original message"
;     (is (= "vigilance"
;            (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
;     (is (= "scones"
;            (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))))
