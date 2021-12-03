with Minerva.Parser.Lexical;

with Minerva.Parser.Subprograms;

package body Minerva.Parser is

   ----------------
   -- Parse_File --
   ----------------

   function Parse_File
     (Path : String)
      return Minerva.Trees.Class_Reference
   is
   begin
      Minerva.Parser.Lexical.Open (Path);
      return Program : constant Minerva.Trees.Class_Reference :=
        Minerva.Parser.Subprograms.Parse_Compilation_Unit
      do
         Minerva.Parser.Lexical.Close;
      end return;
   end Parse_File;

end Minerva.Parser;
