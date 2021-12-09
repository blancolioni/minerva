with Minerva.Names;

with Minerva.Types.Records;

package body Minerva.Trees.Types.Records is

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is null;

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is null;

   function Create
     (Position   : GCS.Positions.File_Position;
      Components : Trees.Declarations.Objects.Sequence.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Components => Components)
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
      Record_Env : constant Minerva.Environment.Environment_Id :=
                     Minerva.Environment.Create_Environment
                       (Minerva.Names.To_Name ("record"), Environment);

      procedure Elaborate_Component
        (Tree : Minerva.Trees.Declarations.Objects.Class_Reference);

      -------------------------
      -- Elaborate_Component --
      -------------------------

      procedure Elaborate_Component
        (Tree : Minerva.Trees.Declarations.Objects.Class_Reference)
      is
      begin
         Tree.Elaborate (Record_Env);
      end Elaborate_Component;

   begin
      This.Components.Iterate (Elaborate_Component'Access);

      This.Tree_Type :=
        Minerva.Types.Class_Reference
          (Minerva.Types.Records.Create
             (This, This.Defining_Name, Record_Env));
   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return "record ...";
   end Image;

end Minerva.Trees.Types.Records;
