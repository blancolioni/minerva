with GCS.Positions;

with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

--  with Minerva.Parser.Blocks;
with Minerva.Parser.Expressions;

with Minerva.Trees.Expressions;
with Minerva.Trees.Statements.Assignment_Statement;
with Minerva.Trees.Statements.Call;
with Minerva.Trees.Statements.If_Statement;
with Minerva.Trees.Statements.Loop_Statement;
with Minerva.Trees.Statements.Null_Statement;

with Minerva.Trees.Statement_Sequences;

package body Minerva.Parser.Statements is

   function Parse_Sequence_Of_Statements
     return Minerva.Trees.Statement_Sequences.Class_Reference;

   ------------------
   -- At_Statement --
   ------------------

   function At_Statement return Boolean is
   begin
      return Tok = Tok_Null
        or else Tok = Tok_Declare
        or else Tok = Tok_Identifier
        or else Tok = Tok_Loop
        or else Tok = Tok_While
        or else Tok = Tok_If;
   end At_Statement;

   ----------------------------------
   -- Parse_Sequence_Of_Statements --
   ----------------------------------

   function Parse_Sequence_Of_Statements
     return Minerva.Trees.Statement_Sequences.Class_Reference
   is
      use Minerva.Trees.Statement_Sequences;
      Statements : constant Class_Reference :=
                     Create_Sequence (Get_Current_Position);
   begin

      if not At_Statement then
         Error ("missing statement");
      end if;

      while At_Statement loop
         Statements.Append (Parse_Statement);
      end loop;

      return Statements;
   end Parse_Sequence_Of_Statements;

   ---------------------
   -- Parse_Statement --
   ---------------------

   function Parse_Statement return Minerva.Trees.Statements.Class_Reference is
      Start_Position : constant GCS.Positions.File_Position :=
                         Get_Current_Position;
      Result : Minerva.Trees.Statements.Class_Reference;
   begin

      if Tok = Tok_Left_Label then
         Scan;
         if Tok = Tok_Identifier then
            --  Minerva.Trees.Append
            --    (Parent,
            --     Minerva.Trees.Create_Node_With_Name
            --       (Tag      => Minerva.Trees.T_Statement_Label,
            --        Location => Get_Current_Position,
            --        Name     => Tok_Text));
            Scan;
            if Tok = Tok_Right_Label then
               Scan;
            else
               Error ("missing '>>'");
            end if;
         else
            Error ("missing label");
         end if;
      end if;

      if not At_Statement then
         Error ("missing statement");
      elsif Tok = Tok_Null then
         declare
            use Minerva.Trees.Statements.Null_Statement;
            Statement : constant Class_Reference := Create (Start_Position);
         begin
            Scan;
            Result := Minerva.Trees.Statements.Class_Reference (Statement);
         end;
      elsif Tok = Tok_Identifier then
         declare
            Target : constant Minerva.Trees.Expressions.Class_Reference :=
                       Minerva.Parser.Expressions.Parse_Expression;
         begin
            if Tok = Tok_Assign then
               Scan;
               declare
                  use Minerva.Trees.Statements.Assignment_Statement;
                  Value : constant Minerva.Trees.Expressions.Class_Reference :=
                            Minerva.Parser.Expressions.Parse_Expression;
                  Statement : constant Class_Reference :=
                                Create (Start_Position, Target, Value);
               begin
                  Result :=
                    Minerva.Trees.Statements.Class_Reference (Statement);
               end;
            else
               declare
                  use Minerva.Trees.Statements.Call;
                  Statement : constant Class_Reference :=
                                Create (Start_Position, Target);
               begin
                  Result :=
                    Minerva.Trees.Statements.Class_Reference (Statement);
               end;
            end if;
         end;
      elsif Tok = Tok_Loop or else Tok = Tok_While then
         declare
            Is_While_Loop : constant Boolean := Tok = Tok_While;
            Condition     : Minerva.Trees.Expressions.Class_Reference;
            Loop_Body     : Minerva.Trees.Statement_Sequences.Class_Reference;
         begin
            if Tok = Tok_While then
               Scan;
               Condition := Minerva.Parser.Expressions.Parse_Expression;
            end if;

            if Tok = Tok_Loop then
               Scan;
            else
               Error ("missing 'loop'");
            end if;

            Loop_Body :=
              Parse_Sequence_Of_Statements;

            if Tok = Tok_End then
               Scan;
               if Tok = Tok_Loop then
                  Scan;
               else
                  Error ("missing 'loop'");
               end if;
            else
               Error ("missing 'end loop'");
            end if;

            if Is_While_Loop then
               Result := Minerva.Trees.Statements.Class_Reference
                 (Minerva.Trees.Statements.Loop_Statement.Create_While_Loop
                    (Position  => Start_Position,
                     Name      => "",
                     Condition => Condition,
                     Sequence  => Loop_Body));
            else
               Result := Minerva.Trees.Statements.Class_Reference
                 (Minerva.Trees.Statements.Loop_Statement.Create_Loop
                    (Position  => Start_Position,
                     Name      => "",
                     Sequence  => Loop_Body));
            end if;
         end;

      elsif Tok = Tok_If then
         declare
            use Minerva.Trees.Statements.If_Statement;
            If_Tree : constant Class_Reference :=
                        Create (Start_Position);

            procedure Parse_Condition_And_Statements;

            ------------------------------------
            -- Parse_Condition_And_Statements --
            ------------------------------------

            procedure Parse_Condition_And_Statements is
               Condition : Minerva.Trees.Expressions.Class_Reference;
               Statements : Minerva.Trees.Statement_Sequences.Class_Reference;
            begin
               if not Minerva.Parser.Expressions.At_Expression then
                  Error ("missing expression");
               else
                  Condition := Minerva.Parser.Expressions.Parse_Expression;
               end if;

               if Tok = Tok_Then then
                  Scan;
               else
                  Error ("missing 'then'");
               end if;

               Statements := Parse_Sequence_Of_Statements;

               If_Tree.Add_Condition_And_Sequence
                 (Condition, Statements);

            end Parse_Condition_And_Statements;

         begin

            Scan;

            Parse_Condition_And_Statements;

            while Tok = Tok_Elsif loop
               Scan;
               Parse_Condition_And_Statements;
            end loop;

            if Tok = Tok_Else then
               Scan;
               If_Tree.Add_Sequence
                 (Parse_Sequence_Of_Statements);
            end if;

            if Tok = Tok_End then
               Scan;
               if Tok = Tok_If then
                  Scan;
               else
                  Error ("missing 'if'");
               end if;
            else
               Error ("missing 'end if'");
            end if;

            Result := Minerva.Trees.Statements.Class_Reference (If_Tree);

         end;
      elsif Tok = Tok_Declare then
         null;
         --  declare
         --     Declare_Node : constant Minerva.Trees.Tree_Node_Id :=
         --                      Minerva.Trees.Create_Node
         --                        (Minerva.Trees.T_Declare_Statement,
         --                         Start_Position);
         --  begin
         --     Minerva.Trees.Append (Parent, Declare_Node);
         --     Scan;
         --     Minerva.Parser.Blocks.Parse_Block (Declare_Node, "");
         --  end;

      else
         raise Constraint_Error with
           "precondition violated";
      end if;

      if Tok = Tok_Semicolon then
         Scan;
      else
         Error ("missing ';'");
      end if;

      return Result;

   end Parse_Statement;

end Minerva.Parser.Statements;
