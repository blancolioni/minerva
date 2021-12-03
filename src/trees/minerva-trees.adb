with Ada.Text_IO;

with Minerva.Entries;
with Minerva.Logging;

package body Minerva.Trees is

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (This : in out Class;
      Tag  : String)
   is
   begin
      Minerva.Errors.Append (This.Errors, This.Position, Tag);
      Minerva.Logging.Log
        (This.Image,
         GCS.Positions.Image (This.Position)
         & ": " & Tag);
   end Add_Error;

   -----------
   -- Check --
   -----------

   procedure Check
     (This        : in out Class;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      This.Log ("checking");
      This.Check_Tree (Environment);
      This.Checked := True;
   end Check;

   ----------------
   -- Check_Tree --
   ----------------

   procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      for Child of Dispatch (This).Children loop
         Child.Log ("checking child");
         Child.Check (Environment);
      end loop;
   end Check_Tree;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (This : Class;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      This.Log ("compiling");
      This.Compile_Tree (Unit);
   end Compile;

   ---------------
   -- Elaborate --
   ---------------

   procedure Elaborate
     (This        : not null access Class;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      This.Log ("elaborating");
      This.Elaborate_Tree (Environment);
      This.Elaborated := True;
   end Elaborate;

   --------------------
   -- Elaborate_Tree --
   --------------------

   procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      for Child of Dispatch (This.all).Children loop
         Child.Log ("elaborating child");
         Child.Elaborate (Environment);
      end loop;
   end Elaborate_Tree;

   ------------
   -- Errors --
   ------------

   function Errors
     (This : Class)
      return Minerva.Errors.Error_Store
   is
   begin
      return This.Errors;
   end Errors;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (This : Class)
      return Minerva.Entries.Constant_Class_Reference
   is
   begin
      return Minerva.Entries.Constant_Class_Reference
        (This.Tree_Entry);
   end Get_Entry;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors
     (This : Class)
      return Boolean
   is
   begin
      return not Minerva.Errors.Is_Empty (This.Errors);
   end Has_Errors;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This     : in out Instance;
      Position : GCS.Positions.File_Position)
   is
   begin
      This.Position := Position;
   end Initialize;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (This    : Class;
      Process : not null access
        procedure (Item : not null access Class))
   is
      procedure Process_Child (Child : not null access Class);

      -------------------
      -- Process_Child --
      -------------------

      procedure Process_Child (Child : not null access Class) is
      begin
         Process (Child);
         Child.Iterate_All (Process);
      end Process_Child;

   begin
      This.Iterate_Children (Process_Child'Access);
   end Iterate_All;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children
     (This    : Class;
      Process : not null access
        procedure (Child : not null access Class))
   is
   begin
      for Child of This.Children loop
         Process (Child);
      end loop;
   end Iterate_Children;

   ---------
   -- Log --
   ---------

   procedure Log
     (This    : Class;
      Message : String)
   is
      use GCS.Positions;
   begin
      if This.Position = Null_Position then
         Minerva.Logging.Log
           (This.Image,
            Message);
      else
         Minerva.Logging.Log
           (GCS.Positions.Image (This.Position) & ": " & This.Image,
            Message);
      end if;
   end Log;

   ---------
   -- Put --
   ---------

   procedure Put
     (This : Class)
   is
      use Ada.Text_IO;

      procedure Put_Child
        (Child  : Class;
         Indent : Count);

      ---------------
      -- Put_Child --
      ---------------

      procedure Put_Child
        (Child  : Class;
         Indent : Count)
      is

         procedure Process (Grandchild : not null access Class);

         -------------
         -- Process --
         -------------

         procedure Process (Grandchild : not null access Class) is
         begin
            Put_Child (Grandchild.all, Indent + 2);
         end Process;

      begin
         Set_Col (Indent);
         Put_Line (Child.Image);
         Child.Iterate_Children (Process'Access);
      end Put_Child;

   begin
      Put_Child (This, 1);
   end Put;

   ---------------
   -- Set_Entry --
   ---------------

   procedure Set_Entry
     (This       : in out Class;
      Tree_Entry : not null access constant Minerva.Entries.Instance'Class)
   is
   begin
      This.Has_Entry := True;
      This.Tree_Entry := Entry_Class_Reference (Tree_Entry);
   end Set_Entry;

end Minerva.Trees;
