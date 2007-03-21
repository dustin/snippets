indexing
   description: "Hello, damnit!";
class HELLO

creation {ANY} 
   make

feature {ANY} 
   
   make is 
      -- print a simple message
      do  
         io.put_string("Hello, damnit...%N");
      end -- make

end -- class HELLO
