with GCS.Positions;

with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

with Minerva.Parser.Declarations;
with Minerva.Parser.Identifiers;

with Minerva.Trees.Declarations.Objects.Sequence;

with Minerva.Trees.Types.Records;
with Minerva.Trees.Types.Named;

package body Minerva.Parser.Types is

   -------------
   -- At_Type --
   -------------

   function At_Type return Boolean is
   begin
      return Tok = Tok_Identifier or else Tok = Tok_Record;
   end At_Type;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type return Minerva.Trees.Types.Class_Reference is
      Position : constant GCS.Positions.File_Position :=
                   Get_Current_Position;
      Result   : Minerva.Trees.Types.Class_Reference;
   begin
      if Tok = Tok_Identifier then
         declare
            Name : constant String :=
                     Minerva.Parser.Identifiers.Parse_Qualified_Identifier;
         begin
            Result :=
              Minerva.Trees.Types.Class_Reference
                (Minerva.Trees.Types.Named.Create_Named_Type
                   (Position => Position,
                    Name     => Name));
         end;
      elsif Tok = Tok_Record then
         Scan;
         declare
            use Minerva.Trees.Declarations.Objects;
            Components : constant Sequence.Class_Reference :=
                           Sequence.Create_Sequence
                             (Get_Current_Position);
         begin
            while Tok = Tok_Identifier loop
               Components.Append
                 (Minerva.Parser.Declarations
                  .Parse_Record_Component_Declaration);
               if Tok = Tok_Semicolon then
                  Scan;
               else
                  Error ("missing ';'");
               end if;
            end loop;

            Result :=
              Minerva.Trees.Types.Class_Reference
                (Minerva.Trees.Types.Records.Create
                   (Position => Position,
                    Components => Components));
         end;
         if Tok = Tok_End then
            Scan;
            if Tok = Tok_Record then
               Scan;
            else
               Error ("missing 'record'");
               Skip_To (Tok_Semicolon);
            end if;
         else
            Error ("missing 'end record'");
            Skip_To (Tok_Semicolon);
         end if;
      else
         raise Constraint_Error with "violated precondition";
      end if;

      return Result;

   end Parse_Type;

end Minerva.Parser.Types;
