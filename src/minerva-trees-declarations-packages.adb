with Minerva.Entries.Packages;
with Minerva.Library;

package body Minerva.Trees.Declarations.Packages is

   ------------
   -- Append --
   ------------

   procedure Append
     (This        : in out Class;
      Declaration : Minerva.Trees.Declarations.Class_Reference)
   is
   begin
      This.Children.Append (Declaration);
   end Append;

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      This.Context.Check (Environment);
      for Declaration of This.Children loop
         Declaration.Check (This.Environment);
      end loop;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
      Last : Natural := 0;
   begin
      return Result : Class_Reference_Array
        (1 .. Natural (This.Children.Length))
      do
         for Child of This.Children loop
            Last := Last + 1;
            Result (Last) := Trees.Class_Reference (Child);
         end loop;
      end return;
   end Children;

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      This.Context.Compile (Code);
      for Declaration of This.Children loop
         Declaration.Compile (Code);
      end loop;
   end Compile_Tree;

   -------------------------
   -- Create_Package_Body --
   -------------------------

   function Create_Package_Body
     (Position      : GCS.Positions.File_Position;
      Context       : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name : String)
      return Class_Reference
   is
      Full_Name : constant Minerva.Names.Minerva_Name :=
                    Minerva.Names.To_Name (Defining_Name);
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
         Is_Body       => True,
         Context       => Context,
         Parent_Names  =>
           Minerva.Names.Join
             (Minerva.Names.Qualifiers (Full_Name)),
         Parent_Entry  => null,
         Defining_Name => Minerva.Names.Base_Name (Full_Name),
         Children      => List_Of_Declarations.Empty_List)
      do
         This.Initialize (Position);
      end return;
   end Create_Package_Body;

   ----------------------------------
   -- Create_Package_Specification --
   ----------------------------------

   function Create_Package_Specification
     (Position      : GCS.Positions.File_Position;
      Context       : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name : String)
      return Class_Reference
   is
      Full_Name : constant Minerva.Names.Minerva_Name :=
                    Minerva.Names.To_Name (Defining_Name);
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Is_Body => False,
         Parent_Names  =>
           Minerva.Names.Join
             (Minerva.Names.Qualifiers (Full_Name)),
         Parent_Entry  => null,
         Defining_Name => Minerva.Names.Base_Name (Full_Name),
         Context       => Context,
         Children      => List_Of_Declarations.Empty_List)
      do
         This.Initialize (Position);
      end return;
   end Create_Package_Specification;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
      Parent_Tree  : Minerva.Trees.Declarations.Class_Reference;

      procedure Elaborate_Parent
        (Parent_Name : Minerva.Names.Minerva_Name);

      -----------------------
      -- Elaborate_Parents --
      -----------------------

      procedure Elaborate_Parent
        (Parent_Name : Minerva.Names.Minerva_Name)
      is
      begin
         Parent_Tree :=
           Minerva.Library.Load_Specification (Parent_Name);

         if not Parent_Tree.Elaborated then
            Parent_Tree.Elaborate (Environment);
         end if;

      end Elaborate_Parent;

   begin

      Minerva.Names.Scan (This.Parent_Names, Elaborate_Parent'Access);

      if This.Is_Body then
         Elaborate_Parent
           (Minerva.Names.Join (This.Parent_Names, This.Defining_Name));
      end if;

      if Parent_Tree /= null then
         This.Parent_Entry := Parent_Tree.Get_Entry;
      end if;

      This.Context.Elaborate (Environment);

      declare
         Full_Name : constant Minerva.Names.Minerva_Name :=
                       Minerva.Names.Join
                         (This.Parent_Names, This.Defining_Name);
      begin
         This.Environment :=
           Minerva.Environment.Create_Environment
             (Full_Name, Environment);
      end;

      This.Set_Entry
        (Minerva.Entries.Class_Reference
          (Minerva.Entries.Packages.Create
             (Declaration  => This,
              Package_Name => This.Defining_Name,
              Container_Name => This.Parent_Names)));

      for Child of This.Children loop
         Child.Elaborate (This.Environment);
      end loop;

   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return "package " & Minerva.Names.Cased_Text (This.Defining_Name);
   end Image;

end Minerva.Trees.Declarations.Packages;
