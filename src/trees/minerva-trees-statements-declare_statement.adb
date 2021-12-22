package body Minerva.Trees.Statements.Declare_Statement is

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Code.Instance)
   is
   begin
      This.Block.Compile (Unit);
   end Compile_Tree;

   ------------
   -- Create --
   ------------

   function Create
     (Position : GCS.Positions.File_Position;
      Block    : Minerva.Trees.Blocks.Class_Reference) return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Block => Block)
      do
         Result.Initialize (Position);
      end return;
   end Create;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      This.Block.Set_Frame_Start
        (Minerva.Environment.Current_Frame_Offset (Environment));
      This.Block.Elaborate (Environment);
   end Elaborate_Tree;

end Minerva.Trees.Statements.Declare_Statement;
