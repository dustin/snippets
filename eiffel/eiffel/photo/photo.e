indexing
   description: "Eiffel port of the photo album stuff.";
class PHOTO

inherit
   MORE_FILE_TOOLS;
   BASE64
      rename encode as base64_encode, decode as base64_decode, init_const as base64_init_const
      redefine make
      end;

creation {ANY}
   make

feature {ANY} -- Creation

   make is
      do
         -- Set up the database stuff
         !!db.make;
         db.set_host("bleu");
         db.set_dbname("photo");
         base64_init_const;
      end -- make

feature {ANY} -- Misc stuff

   getuid: INTEGER is
      require
         has_remote_user: get_environment_variable("REMOTE_USER") /= Void;
      local
         a: ARRAY[STRING];
         b: BOOLEAN;
         query: STRING;
         retry_attempts: INTEGER;
      do
         Result := - 1;
         check
            db.connect;
         end;
         !!query.copy("select getwwwuser(");
         query.append(db.quote(get_environment_variable("REMOTE_USER")));
         query.append(")");
         if db.query(query) then
            if db.get_row then
               a := db.last_row;
               Result := a.item(0).to_integer;
            end;
         end;
      rescue
         if retry_attempts < max_retry_attempts then
            retry_attempts := retry_attempts + 1;
            retry;
         end;
      end -- getuid

   cacheimage(which: INTEGER) is
      -- Cache an image
      local
         a: ARRAY[STRING];
         b: BOOLEAN;
         query, path, image_data: STRING;
         retry_attempts: INTEGER;
      do
         if not image_cache_exists(which) then
            -- Get the image from the database
            path := photo_cache_fn(which);
            query := get_image_data_query(which);
            check
               db.connect;
            end;
            !!image_data.make(65535);
            image_data.clear;
            if db.query(query) then
               from
                  b := db.get_row;
               until
                  b = false
               loop
                  a := db.last_row;
                  image_data.append(base64_decode(a.item(2)));
                  b := db.get_row;
               end;
            end;
            check
               image_data.count > 0;
            end;
            do_cache(path,image_data);
         end;
      ensure
         image_cache_exists(which);
      rescue
         if retry_attempts < max_retry_attempts then
            -- Make sure we don't try too many times.
            retry_attempts := retry_attempts + 1;
            retry;
         end;
      end -- cacheimage

   image_cache_exists(which: INTEGER): BOOLEAN is
      -- Check to see if an image is cached.
      local
         s: STRING;
      do
         s := photo_cache_fn(which);
         Result := path_exists(s);
      end -- image_cache_exists

feature {PHOTO}
   -- Internal functions

   do_cache(path, data: STRING) is
      require
         has_path: path /= Void;
         has_data: data /= Void;
      local
         f: STD_FILE_WRITE;
      do
         ensurepath(path);
         !!f.connect_to(path);
         f.put_string(data);
         f.disconnect;
      ensure
         path_exists(path);
      end -- do_cache

   photo_cache_fn(which: INTEGER): STRING is
      do
         !!Result.copy(cache_base);
         Result.append(which.to_string);
      end -- photo_cache_fn

   get_image_data_query(which: INTEGER): STRING is
      require
         positive: which >= 0;
      do
         !!Result.copy("select * from image_store where id=");
         Result.append(which.to_string);
         Result.append(" order by line");
      end -- get_image_data_query

feature {PHOTO}
   -- Internal data structures

   db: PG;

   cache_base: STRING is "/tmp/photo/cache/";

   max_retry_attempts: INTEGER is 3;

end -- class PHOTO
