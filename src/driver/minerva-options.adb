with WL.Command_Line;

package body Minerva.Options is

   pragma Style_Checks (Off);

   function Execute return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("execute", ' ');
   end Execute;

   function Compile_Only return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("compile-only", ' ');
   end Compile_Only;

   function Target return String is
   begin
      return WL.Command_Line.Find_Option
               ("target", ' ');
   end Target;

   function Trace_Environment return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("trace-environment", ' ');
   end Trace_Environment;

   function Trace_Stages return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("trace-stages", ' ');
   end Trace_Stages;

   function Write_Listing return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("write-listing", ' ');
   end Write_Listing;

end Minerva.Options;
