with Minerva.Names;

package body Minerva.Trees.Declarations is

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect
     (This   : in out Class;
      Aspect : not null Minerva.Trees.Aspects.Class_Reference)
   is
   begin
      This.Aspects.Append (Aspect);
   end Add_Aspect;

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      Parent (This).Check_Tree (Environment);
      for Aspect of This.Aspects loop
         Aspect.Check (Environment);
      end loop;
   end Check_Tree;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      Parent (This.all).Elaborate_Tree (Environment);
      for Aspect of This.Aspects loop
         Aspect.Elaborate (Environment);
      end loop;
   end Elaborate_Tree;

   -----------------
   -- Find_Aspect --
   -----------------

   function Find_Aspect
     (This : Class;
      Name : String)
      return Minerva.Trees.Aspects.Class_Reference
   is
      use type Minerva.Names.Minerva_Name;
   begin
      for Aspect of This.Aspects loop
         if Aspect.Name = Name then
            return Aspect;
         end if;
      end loop;
      return null;
   end Find_Aspect;

   ----------------
   -- Has_Aspect --
   ----------------

   function Has_Aspect
     (This : Class;
      Name : String)
      return Boolean
   is
      use type Minerva.Names.Minerva_Name;
   begin
      return (for some Aspect of This.Aspects => Aspect.Name = Name);
   end Has_Aspect;

   ---------------------
   -- Iterate_Entries --
   ---------------------

   procedure Iterate_Entries
     (This    : Instance;
      Process : not null access
        procedure (Item : Minerva.Entries.Constant_Class_Reference))
   is
   begin
      Process (This.Get_Entry);
   end Iterate_Entries;

end Minerva.Trees.Declarations;
