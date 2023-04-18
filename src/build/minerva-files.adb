with Ada.Directories;
with Ada.Text_IO;

with Minerva.Options;
with Minerva.Paths;

package body Minerva.Files is

   ---------------
   -- Find_File --
   ---------------

   function Find_File (File_Name : String) return String is
   begin
      if Ada.Directories.Exists (File_Name) then
         return File_Name;
      elsif Ada.Directories.Exists
        (Minerva.Paths.Config_File ("rts/" & File_Name))
      then
         return Minerva.Paths.Config_File ("rts/" & File_Name);
      elsif Ada.Directories.Exists
        (Minerva.Paths.Config_File
           ("arch/" & Minerva.Options.Target
            & "/" & File_Name))
      then
         return Minerva.Paths.Config_File
           ("arch/" & Minerva.Options.Target
            & "/" & File_Name);
      else
         if False then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot find file: " & File_Name);
         end if;
         return "";
      end if;
   end Find_File;

end Minerva.Files;
