package body Minerva.Trees.Sequences is

   ------------
   -- Append --
   ------------

   procedure Append
     (This    : in out Class;
      Element : not null access Element_Instance'Class)
   is
   begin
      This.List.Append (Element_Reference (Element));
   end Append;

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      for Element of This.List loop
         Element.Check (Environment);
      end loop;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children
     (This : Instance)
      return Class_Reference_Array
   is
      Index : Natural := 0;
   begin
      return Arr : Class_Reference_Array (1 .. Natural (This.List.Length)) do
         for Element of This.List loop
            Index := Index + 1;
            Arr (Index) := Trees.Class_Reference (Element);
         end loop;
      end return;
   end Children;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      for Element of This.List loop
         Element.Compile (Unit);
      end loop;
   end Compile_Tree;

   ---------------------
   -- Create_Sequence --
   ---------------------

   function Create_Sequence
     (Position : GCS.Positions.File_Position)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize (Position);
      end return;
   end Create_Sequence;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return "sequence";
   end Image;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This    : Class;
      Process : not null access
        procedure (Element : Element_Reference))
   is
   begin
      for Element of This.List loop
         Process (Element);
      end loop;
   end Iterate;

end Minerva.Trees.Sequences;
