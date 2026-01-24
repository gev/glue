String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time
(
    (def (inc counter) (set counter.value (+ counter.value 1)))
    (def (dec counter) (set counter.value (- counter.value 1)))

    (def my-counter (reactive-counter 0))

    (text
        :content "Hello Glue!"
        :color colors.grey
        :size 32)
   
    (reactive-widget my-counter
        (text
            :content my-counter.value
            :color colors.blue
            :size 128))
   
    (row
        :main-axis-align main-axis-alignment.center
        :children (
            (button
                :label "Increment"
                :on-tap (\\ () (inc my-counter)))
            (button
                :label "Decrement"
                :on-tap (\\ () (dec my-counter)))))
)
''';
