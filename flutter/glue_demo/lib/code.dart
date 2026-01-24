String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time

(
    (def my-counter (reactive-counter 0))

    (reactive-widget my-counter
        (text
            :content my-counter.value
            :color colors.blue
            :font-size 48
        )
    )
)
''';
