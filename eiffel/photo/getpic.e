indexing
   description: "Photo test...";
--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: getpic.e,v 1.4 1999/05/28 01:27:38 dustin Exp $
--
class PHOTO_GET

creation {ANY} 
   make

feature {ANY} 
   
   make is 
      local 
         b: BOOLEAN;
         a: ARRAY[STRING];
         db: PG;
		 decoded: STRING;
		 base64: BASE64;
      do  
         !!db.make;
		 !!base64.make;
		 db.set_dbname("photo");
		 db.set_host("bleu");
         if not db.connect then 
            io.put_string("NOT Connected%N");
         end; 
         if db.query("select * from image_store where id=501 order by line")
		 then 
            from 
               b := db.get_row;
            until 
               b = false
            loop 
               a := db.last_row;
			   decoded:=base64.decode(a.item(2));
			   io.put_string(decoded);
               b := db.get_row;
            end; 
         end; 
      end -- make

end
