with Minerva.Entries.Types;

package body Minerva.Trees.Declarations.Types is

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This : in out Instance; Environment : Minerva.Environment.Environment_Id)
   is null;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return Maybe (This.Type_Definition);
   end Children;

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is null;

   ------------
   -- Create --
   ------------

   function Create
     (Position        : GCS.Positions.File_Position;
      Defining_Name   : String;
      Type_Definition : not null access Minerva.Trees.Types.Class)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
           Type_Name => Minerva.Names.To_Name (Defining_Name),
           Type_Definition =>
             Minerva.Trees.Types.Class_Reference (Type_Definition))
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
      This.Type_Definition.Elaborate (Environment);
      This.Set_Entry
        (Minerva.Entries.Types.Create
           (Declaration => This,
            Name        => This.Type_Name,
            Definition  => This.Type_Definition.Get_Type));
      Minerva.Environment.Insert (Environment, This.Get_Entry);
   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return "type " & Minerva.Names.Cased_Text (This.Type_Name);
   end Image;

end Minerva.Trees.Declarations.Types;
