with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

with Minerva.Parser.Declarations;
with Minerva.Parser.Statements;

with Minerva.Trees.Declarations;
with Minerva.Trees.Statements;

package body Minerva.Parser.Blocks is

   -----------------
   -- Parse_Block --
   -----------------

   function Parse_Block
     (Name    : String)
      return Minerva.Trees.Blocks.Class_Reference
   is
      Block : constant Minerva.Trees.Blocks.Class_Reference :=
                Minerva.Trees.Blocks.Create_Block
                  (Position => Get_Current_Position,
                   Name     => Name);
   begin
      while Minerva.Parser.Declarations.At_Declaration loop
         declare
            use type Minerva.Trees.Declarations.Class_Reference;
            subtype Dec_Reference is
              Minerva.Trees.Declarations.Class_Reference;
            Declaration : constant Dec_Reference :=
                            Minerva.Parser.Declarations.Parse_Declaration
                              (Context => Minerva.Parser.Block);
         begin
            if Declaration /= null then
               Block.Add_Declaration (Declaration);
            end if;
         end;
      end loop;

      if Tok = Tok_Begin then
         Scan;
      else
         Error ("missing 'begin'");
      end if;

      if not Minerva.Parser.Statements.At_Statement then
         Error ("missing statement");
      end if;

      while Minerva.Parser.Statements.At_Statement loop
         declare
            use type Minerva.Trees.Statements.Class_Reference;
            Statement : constant Minerva.Trees.Statements.Class_Reference :=
                            Minerva.Parser.Statements.Parse_Statement;
         begin
            if Statement /= null then
               Block.Add_Statement (Statement);
            end if;
         end;
      end loop;

      if Tok = Tok_End then
         Scan;
      else
         Error ("missing 'end'");
      end if;

      if Name /= "" then
         if Tok = Tok_Identifier then
            if Name /= Tok_Text then
               Error ("end " & Name
                      & " required");
            end if;
            Scan;
         else
            Error ("end " & Name
                   & " required");
         end if;
      end if;

      return Block;

   end Parse_Block;

end Minerva.Parser.Blocks;
