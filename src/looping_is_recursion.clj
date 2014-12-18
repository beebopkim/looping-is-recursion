(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc e]
                 (if (zero? e)
                   acc
                   (recur (* acc base) (dec e))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc s]
                 (if (empty? s)
                   acc
                   (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc s1 s2]
                 (if (and (empty? s1) (empty? s2))
                   true
                   (if (or (empty? s1) (empty? s2) (not acc))
                     false
                     (recur (= (first s1) (first s2))
                            (rest s1) (rest s2)))))]
    (helper true seq1 seq2)))

(defn loopy-factorial [down-from]
  (loop [acc 1
         n down-from]
    (if (zero? n)
      acc
      (recur (* acc n) (dec n)))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         s a-seq]
    (if (empty? s) nil
      (if (pred (first s)) acc
        (recur (inc acc) (rest s))))))

(defn avg [a-seq]
  (loop [cnt 0
         total 0
         s a-seq]
    (if (empty? s)
      (if (== cnt 0) nil
        (/ total cnt))
      (recur (inc cnt) (+ total (first s)) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (if (empty? s)
      acc
      (recur (toggle acc (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [fibo-n-2 0
         fibo-n-1 1
         nn 2]
    (if (= n 0) fibo-n-2
      (if (= n 1) fibo-n-1
        (if (= n nn) (+ fibo-n-2 fibo-n-1)
          (recur fibo-n-1 (+ fibo-n-2 fibo-n-1) (inc nn)))))))

(defn cut-at-repetition [a-seq]
  (loop [a-result []
         a-set #{}
         s a-seq]
    (if (empty? s)
      a-result
      (if (contains? a-set (first s))
        (recur a-result a-set (rest s))
        (recur (conj a-result (first s))
               (conj a-set (first s))
               (rest s))))))

