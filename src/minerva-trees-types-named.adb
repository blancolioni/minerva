with Minerva.Entries;
with Minerva.Primitives;

package body Minerva.Trees.Types.Named is

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is null;

   overriding procedure Compile_Tree
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is null;

   -----------------------
   -- Create_Named_Type --
   -----------------------

   function Create_Named_Type
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Name => Minerva.Names.To_Name (Name))
      do
         Result.Initialize (Position);
      end return;
   end Create_Named_Type;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
      Exists : constant Boolean :=
                 Minerva.Environment.Exists (Environment, This.Name);
      Element : constant Minerva.Entries.Constant_Class_Reference :=
                  (if Exists
                   then Minerva.Environment.Get
                     (Environment => Environment,
                      Name        => This.Name)
                   else null);
   begin
      if not Exists then
         This.Add_Error ("undefined");
         This.Tree_Type := Minerva.Primitives.Standard_Boolean;
      elsif not Element.Is_Type_Entry then
         This.Add_Error ("subtype-mark-required");
         This.Tree_Type := Minerva.Primitives.Standard_Boolean;
      else
         This.Tree_Type := Element.Entry_Type;
      end if;
   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return Minerva.Names.Cased_Text (This.Name);
   end Image;

end Minerva.Trees.Types.Named;
