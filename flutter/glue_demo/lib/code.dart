String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time
((def 
   (hello message color) 
   (text :content message
         :color color
         :size 64
         :weight font-weight.bold))
       
   (hello "Hello World!" colors.blue)
   (hello "Hello Glue!" colors.blue)
   (button :label "Press me"))
''';
