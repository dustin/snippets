indexing
   description: "Replicate the Mr. Modem database...";
version: "$Revision: 1.7 $";
class MREPLICATE

creation {ANY}
   make

feature {ANY}

   make is
      local
         s, d: PG;
         rep: PG_REPLICATE;
      do
         !!s.make;
         s.set_dbname("events");
         s.connect;
         !!d.make;
         d.set_dbname("event_tmp");
         d.connect;
         !!rep.make(s,d);
         rep.full;
      end -- make

end -- class MREPLICATE
