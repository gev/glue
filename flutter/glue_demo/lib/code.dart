String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time

(
    (def my-counter (reactive-counter 0))

    (def (demo counter)
        (reactive-widget counter (
            (text
                :content counter.value
                :color colors.blue
                :font-size 48
            )
            (button
                :label "Increment"
                :on-press (\\ () (counter.increment 1))
            )
            (button
                :label "Decrement"
                :on-press (\\ () (counter.decrement 1))
            )
        ))
    )
    
    (text 
        :content "Hello Glue!" 
        :color colors.grey 
        :font-size 24
    )
    (demo my-counter)
)
''';
