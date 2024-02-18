package Minerva.Paths is

   Config_Path : constant String :=
     "./share/minerva";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Minerva.Paths;
