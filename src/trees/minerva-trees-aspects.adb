package body Minerva.Trees.Aspects is

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      null;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return (1 => Trees.Class_Reference (This.Value));
   end Children;

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      null;
   end Compile_Tree;

   -------------------
   -- Create_Aspect --
   -------------------

   function Create_Aspect
     (Position : GCS.Positions.File_Position;
      Name     : String;
      Value    : not null Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
           Name  => Minerva.Names.To_Name (Name),
           Value => Value)
      do
         Result.Initialize (Position);
      end return;
   end Create_Aspect;

   -------------------
   -- Create_Aspect --
   -------------------

   function Create_Aspect
     (Position : GCS.Positions.File_Position; Name : String)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
           Name  => Minerva.Names.To_Name (Name),
           Value => null)
      do
         Result.Initialize (Position);
      end return;
   end Create_Aspect;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
      use type Minerva.Trees.Expressions.Class_Reference;
   begin
      if This.Value /= null then
         This.Value.Elaborate (Environment);
         This.Value.Check (Environment);
      end if;
   end Elaborate_Tree;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value (Aspect : Class) return Boolean is
      use type Minerva.Trees.Expressions.Class_Reference;
   begin
      return Aspect.Value /= null;
   end Has_Value;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
      Name : constant String :=
               Minerva.Names.Cased_Text (This.Name);
   begin
      if This.Has_Value then
         return Name & " => " & This.Value.Image;
      else
         return Name;
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value
     (Aspect : Class)
      return not null Minerva.Trees.Expressions.Class_Reference
   is
   begin
      return Aspect.Value;
   end Value;

end Minerva.Trees.Aspects;
