with Minerva.Types.Void;

package body Minerva.Trees.Statements.Call is

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      This.Call.Add_Possible_Type (Minerva.Types.Void.Void_Type);
      This.Call.Check (Environment);
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return (1 => Trees.Class_Reference (This.Call));
   end Children;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      This.Call.Push (Unit);
   end Compile_Tree;

   ------------
   -- Create --
   ------------

   function Create
     (Position   : GCS.Positions.File_Position;
      Expression : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Call => Expression)
      do
         Result.Initialize (Position);
      end return;
   end Create;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return This.Call.Image;
   end Image;

end Minerva.Trees.Statements.Call;
