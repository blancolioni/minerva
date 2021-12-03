package body Minerva.Trees.Identifiers is

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is null;

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is null;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return Null_Class_Reference_Array;
   end Children;

   -------------------------
   -- Defining_Identifier --
   -------------------------

   function Defining_Identifier
     (Position : GCS.Positions.File_Position; Identifier : String)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
         Defining    => True,
         Identifier  => Minerva.Names.To_Name (Identifier))
      do
         This.Initialize (Position);
      end return;
   end Defining_Identifier;

   --------------------------
   -- Identifier_Reference --
   --------------------------

   function Identifier_Reference
     (Position : GCS.Positions.File_Position; Identifier : String)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
         Defining    => False,
         Identifier  => Minerva.Names.To_Name (Identifier))
      do
         This.Initialize (Position);
      end return;
   end Identifier_Reference;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return This.Cased_Text;
   end Image;

end Minerva.Trees.Identifiers;
