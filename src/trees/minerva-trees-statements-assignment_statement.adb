package body Minerva.Trees.Statements.Assignment_Statement is

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      This.Target.Check (Environment);
      if This.Target.Has_Type then
         This.Value.Add_Possible_Type (This.Target.Get_Type);
         This.Value.Check (Environment);
      end if;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return (Trees.Class_Reference (This.Target),
              Trees.Class_Reference (This.Value));
   end Children;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Code.Instance)
   is
   begin
      This.Target.Push_Address (Unit);
      This.Value.Push (Unit);
      Unit.Store;
   end Compile_Tree;

   ------------
   -- Create --
   ------------

   function Create
     (Position : GCS.Positions.File_Position;
      Target   : Minerva.Trees.Expressions.Class_Reference;
      Value    : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Target => Target, Value => Value)
      do
         Result.Initialize (Position);
      end return;
   end Create;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return This.Target.Image & " := " & This.Value.Image;
   end Image;

end Minerva.Trees.Statements.Assignment_Statement;
