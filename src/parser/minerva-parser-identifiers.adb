with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

package body Minerva.Parser.Identifiers is

   --------------------------------
   -- Parse_Qualified_Identifier --
   --------------------------------

   function Parse_Qualified_Identifier return String is
      Id : constant String := Tok_Raw_Text;
   begin
      Scan;
      if Tok = Tok_Dot then
         Scan;
         if Tok = Tok_Identifier then
            return Id & "." & Parse_Qualified_Identifier;
         else
            Error ("identifier expected");
            return Id;
         end if;
      else
         return Id;
      end if;
   end Parse_Qualified_Identifier;

end Minerva.Parser.Identifiers;
