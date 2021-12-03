with WL.Command_Line;

package body Minerva.Options is

   pragma Style_Checks (Off);

   function Execute return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("execute", ' ');
   end Execute;

   function Write_Listing return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("write-listing", ' ');
   end Write_Listing;

end Minerva.Options;
