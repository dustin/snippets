indexing
   description: "More Eiffel File Tools";
class MORE_FILE_TOOLS

inherit
   FILE_TOOLS;

feature {ANY} -- More useful file tools

   ensurepath(path: STRING) is
      -- Verify that the necessary directories exist to create the given
      -- path
      require
         has_path: path /= Void;
         not_root: not path.is_equal("/");
      local
         dir: STRING;
      do
         dir := dirname(path);
         if not path_exists(dir) then
            ensurepath(dir);
            if not path_exists(dir) then
               mkdir(dir);
            end;
         end;
         check -- make sure it worked
            path_exists(dir);
         end;
      end -- ensurepath

   dirname(of: STRING): STRING is
      -- Binding to c dirname
      require
         has_path: of /= Void;
      local
         s: STRING;
      do
         !!s.copy(of);
         !!Result.from_external_copy(c_dirname(s.to_external));
      ensure
         Result /= Void;
      end -- dirname

   path_exists(path: STRING): BOOLEAN is
      -- Verify that something exists in a given path
      require
         has_path: path /= Void;
      do
         Result := c_access(path.to_external,0) = 0;
      end -- path_exists

feature {MORE_FILE_TOOLS}
   -- C stuff

   c_dirname(in: POINTER): POINTER is
      -- Binding to the C dirname routine
      external "C_WithoutCurrent"
      alias "dirname"
      end -- c_dirname

   c_access(in: POINTER; mode: INTEGER): INTEGER is
      external "C_WithoutCurrent"
      alias "access"
      end -- c_access

end -- class MORE_FILE_TOOLS
