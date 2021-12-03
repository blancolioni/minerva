with Minerva.Environment;
with Minerva.Primitives;

package body Minerva.Trees.Statements.Loop_Statement is

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      case This.Form is
         when Basic_Loop =>
            null;
         when For_Loop =>
            null;
         when Iterator_Loop =>
            null;
         when While_Loop =>
            This.Condition.Add_Possible_Type
              (Minerva.Primitives.Standard_Boolean);
            This.Condition.Check (Environment);
      end case;
      This.Loop_Body.Check (Environment);
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children
     (This : Instance)
      return Class_Reference_Array
   is
   begin
      return (case This.Form is
                 when Basic_Loop    => Null_Class_Reference_Array,
                 when For_Loop      => Null_Class_Reference_Array,
                 when Iterator_Loop => Null_Class_Reference_Array,
                 when While_Loop    =>
                (1 => Trees.Class_Reference (This.Condition)))
        & Trees.Class_Reference (This.Loop_Body);
   end Children;

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
      Loop_Label : constant Positive := Unit.Next_Label;
      Out_Label  : constant Positive := Unit.Next_Label;
   begin
      Unit.Label (Loop_Label);

      case This.Form is
         when Basic_Loop =>
            null;
         when For_Loop =>
            null;
         when Iterator_Loop =>
            null;
         when While_Loop =>
            This.Condition.Push (Unit);
            Unit.Jump (Out_Label, Tagatha.C_Equal);
      end case;
      This.Loop_Body.Compile (Unit);
      Unit.Jump (Loop_Label, Tagatha.C_Always);
      Unit.Label (Out_Label);
   end Compile_Tree;

   -----------------
   -- Create_Loop --
   -----------------

   function Create_Loop
     (Position : GCS.Positions.File_Position;
      Name     : String;
      Sequence : Minerva.Trees.Statement_Sequences.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
         Form      => Basic_Loop,
         Name      => Minerva.Names.To_Name (Name),
         Condition => null,
         Loop_Body => Sequence)
      do
         Result.Initialize (Position);
      end return;
   end Create_Loop;

   -----------------------
   -- Create_While_Loop --
   -----------------------

   function Create_While_Loop
     (Position  : GCS.Positions.File_Position; Name : String;
      Condition : Minerva.Trees.Expressions.Class_Reference;
      Sequence  : Minerva.Trees.Statement_Sequences.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
         Form      => While_Loop,
         Name      => Minerva.Names.To_Name (Name),
         Condition => Condition,
         Loop_Body => Sequence)
      do
         Result.Initialize (Position);
      end return;
   end Create_While_Loop;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return (case This.Form is
                 when Basic_Loop => "",
                 when For_Loop   => "for .. in .. ",
                 when Iterator_Loop => "for .. of .. ",
                 when While_Loop    => "while " & This.Condition.Image & " ")
         & "loop " & This.Loop_Body.Image;
   end Image;

end Minerva.Trees.Statements.Loop_Statement;
