with GCS.Positions;

with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

with Minerva.Trees.Expressions;

with Minerva.Parser.Expressions;

package body Minerva.Parser.Aspects is

   ------------------
   -- Parse_Aspect --
   ------------------

   function Parse_Aspect
     return Minerva.Trees.Aspects.Class_Reference
   is
      pragma Assert (Tok = Tok_Identifier);
      Start     : constant GCS.Positions.File_Position := Get_Current_Position;
      Name      : constant String := Tok_Text;
   begin
      Scan;
      if Tok = Tok_Right_Arrow then
         Scan;

         declare
            Value : constant Minerva.Trees.Expressions.Class_Reference :=
                      Minerva.Parser.Expressions.Parse_Expression;
         begin
            return Minerva.Trees.Aspects.Create_Aspect
              (Position => Start,
               Name     => Name,
               Value    => Value);
         end;
      else
         return Minerva.Trees.Aspects.Create_Aspect
           (Position => Start,
            Name     => Name);
      end if;
   end Parse_Aspect;

end Minerva.Parser.Aspects;
