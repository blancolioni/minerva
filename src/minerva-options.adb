with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Parse_Args;

package body Minerva.Options is

   use Parse_Args;

   AP : Argument_Parser;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Source_File_Vector : String_Vectors.Vector;

   ----------
   -- Arch --
   ----------

   function Arch return String is
   begin
      return AP.String_Value ("target architecture");
   end Arch;

   ----------------------------
   -- Iterate_Tail_Arguments --
   ----------------------------

   procedure Iterate_Tail_Arguments
     (Process : not null access
        procedure (Argument : String))
   is
   begin
      for File_Name of Source_File_Vector loop
         Process (File_Name);
      end loop;
   end Iterate_Tail_Arguments;

   ----------
   -- Load --
   ----------

   function Load return Boolean is
   begin

      AP.Add_Option
        (Make_String_Option ("a.out"),
         "object file name", 'o', "object-name",
         "Write output to the given path (default: a.out)");

      AP.Add_Option
        (Make_String_Option ("pdp-11"),
         "target architecture", 'a', "arch",
         "Target architecture: pdp-11, aqua, or 6502 (default: pdp-11)");

      AP.Add_Option
        (Make_Boolean_Option (False),
         "write listing", 'l', "write-listing",
         "Write a listing file");

      AP.Allow_Tail_Arguments ("source files ...");

      AP.Parse_Command_Line;

      if not AP.Parse_Success then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            AP.Parse_Message);
         return False;
      end if;

      for File_Name of AP.Tail loop
         Source_File_Vector.Append (File_Name);
      end loop;

      return True;

   end Load;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name return String is
   begin
      return AP.String_Value ("object file name");
   end Output_File_Name;

   -------------------
   -- Write_Listing --
   -------------------

   function Write_Listing return Boolean is
   begin
      return AP.Boolean_Value ("write listing");
   end Write_Listing;

end Minerva.Options;
