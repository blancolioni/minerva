with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

with Minerva.Trees.Types.Named;

package body Minerva.Parser.Types is

   -------------
   -- At_Type --
   -------------

   function At_Type return Boolean is
   begin
      return Tok = Tok_Identifier;
   end At_Type;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type return Minerva.Trees.Types.Class_Reference is
      Name : constant String := Tok_Text;
      Result : constant Minerva.Trees.Types.Named.Class_Reference :=
                 Minerva.Trees.Types.Named.Create_Named_Type
                   (Position => Get_Current_Position,
                    Name     => Name);
   begin

      Scan;

      return Minerva.Trees.Types.Class_Reference (Result);
   end Parse_Type;

end Minerva.Parser.Types;
