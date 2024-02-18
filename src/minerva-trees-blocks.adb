package body Minerva.Trees.Blocks is

   ---------------------
   -- Add_Declaration --
   ---------------------

   procedure Add_Declaration
     (This        : in out Class;
      Declaration : not null Minerva.Trees.Declarations.Class_Reference)
   is
   begin
      This.Declarations.Append (Declaration);
   end Add_Declaration;

   -------------------
   -- Add_Statement --
   -------------------

   procedure Add_Statement
     (This      : in out Class;
      Statement : not null Minerva.Trees.Statements.Class_Reference)
   is
   begin
      This.Statements.Append (Statement);
   end Add_Statement;

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      for Declaration of This.Declarations loop
         Declaration.Check (This.Environment);
      end loop;
      for Statement of This.Statements loop
         Statement.Check (This.Environment);
      end loop;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children
     (This : Instance)
      return Class_Reference_Array
   is
      Decl_Count : constant Natural := Natural (This.Declarations.Length);
      Stat_Count : constant Natural := Natural (This.Statements.Length);
      Result     : Class_Reference_Array (1 .. Decl_Count + Stat_Count);
      Index      : Natural := 0;
   begin
      for Declaration of This.Declarations loop
         Index := Index + 1;
         Result (Index) := Minerva.Trees.Class_Reference (Declaration);
      end loop;
      for Statement of This.Statements loop
         Index := Index + 1;
         Result (Index) := Minerva.Trees.Class_Reference (Statement);
      end loop;
      return Result;
   end Children;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      for Declaration of This.Declarations loop
         Declaration.Compile (Code);
      end loop;
      for Statement of This.Statements loop
         Statement.Compile (Code);
      end loop;
   end Compile_Tree;

   ------------------
   -- Create_Block --
   ------------------

   function Create_Block
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
           Name => Minerva.Names.To_Name (Name),
         Frame_Offset => 0,
         Environment => <>,
         Declarations => <>,
         Statements   => <>)
      do
         Result.Initialize (Position);
      end return;
   end Create_Block;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      This.Environment :=
        Minerva.Environment.Create_Environment
          (This.Name, Environment);
      Minerva.Environment.Set_Frame_Offset
        (This.Environment, This.Frame_Offset);
      for Declaration of This.Declarations loop
         Declaration.Elaborate (This.Environment);
      end loop;
      for Statement of This.Statements loop
         Statement.Elaborate (This.Environment);
      end loop;
   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      if This.Has_Name then
         return This.Cased_Text & ": block";
      else
         return "block";
      end if;
   end Image;

   ---------------------
   -- Set_Frame_Start --
   ---------------------

   procedure Set_Frame_Start
     (This   : in out Class;
      Offset : Natural)
   is
   begin
      This.Frame_Offset := Offset;
   end Set_Frame_Start;

end Minerva.Trees.Blocks;
