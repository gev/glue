String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time
(
    (def (inc counter n) (\\ () (modify counter (\\ (x) (+ x n)))))
    (def (dec counter n) (\\ () (modify counter (\\ (x) (- x n)))))

    (def initial-value 0)
    (def my-counter "my-counter")
    (def app-store create-store)
    (put my-counter (state initial-value) app-store)

    (def (counter-display current-value) (
        (def size (+ current-value 100))
        (text 
            :content current-value 
            :style (text-style :font-size size :color colors.blue))))

    (def (greeting message) 
        (text 
            :content message 
            :style (text-style :font-size 32 :color colors.grey)))

    (def (action title on-tap) 
        (button :child (text :content title) :on-pressed on-tap))

    (def (demo-screen props) (
        (def counter (get my-counter app-store))
        (column
            :cross-axis-alignment cross-axis-alignment.center
            :children (
                (greeting props.message)
                (listen counter counter-display)
                (row 
                    :main-axis-alignment main-axis-alignment.center 
                    :children (
                        (action "Increment" (inc counter props.amount))
                        (action "Decrement" (dec counter props.amount)))))))
    )

    (demo-screen 
        :message "Hello Glue!" 
        :amount 1)
)
''';
