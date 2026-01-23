String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time

((def 
   (hello message color) 
   (text :content message
         :color color
         :size 32
   ))
       
   (hello "Hello World!" colors.blue)
   (hello "Glue Demo. Live UI editor" colors.grey)
   (row 
      :main-axis-align main-axis-alignment.center
      :children (
         (button :label "Press me")
         (button :label "Tap me")
   )))
''';
