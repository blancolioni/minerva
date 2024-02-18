package Minerva.Options is

   function Load return Boolean;

   function Arch return String;
   function Output_File_Name return String;
   function Write_Listing return Boolean;

   procedure Iterate_Tail_Arguments
     (Process : not null access
        procedure (Argument : String));

end Minerva.Options;
