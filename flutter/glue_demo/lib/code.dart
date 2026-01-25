String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time
(
    (def counter (state 0))

    (text
        :content "Hello Glue!"
        :color colors.grey
        :size 32)

    (listen counter
        (text
            :content counter.value
            :color colors.blue
            :size 128))
   
    (row
        :main-axis-align main-axis-alignment.center
        :children (
            (button
                :label "Increment"
                :on-tap (\\ () (set counter (+ counter.value 1))))
            (button
                :label "Decrement"
                :on-tap (\\ () (set counter (- counter.value 1))))))
)
''';
