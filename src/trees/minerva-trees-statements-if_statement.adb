with Tagatha.Labels;

with Minerva.Primitives;

package body Minerva.Trees.Statements.If_Statement is

   --------------------------------
   -- Add_Condition_And_Sequence --
   --------------------------------

   procedure Add_Condition_And_Sequence
     (This      : in out Class;
      Condition :        Minerva.Trees.Expressions.Class_Reference;
      Sequence  :        Minerva.Trees.Statement_Sequences.Class_Reference)
   is
   begin
      This.Elements.Append (Element_Type'
                              (Condition => Condition,
                               Sequence  => Sequence));
   end Add_Condition_And_Sequence;

   ------------------
   -- Add_Sequence --
   ------------------

   procedure Add_Sequence
     (This     : in out Class;
      Sequence :        Minerva.Trees.Statement_Sequences.Class_Reference)
   is
   begin
      This.Elements.Append (Element_Type'
                              (Condition => null,
                               Sequence  => Sequence));
   end Add_Sequence;

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This : in out Instance; Environment : Minerva.Environment.Environment_Id)
   is
      use type Minerva.Trees.Expressions.Class_Reference;
   begin
      for Element of This.Elements loop
         if Element.Condition /= null then
            Element.Condition.Add_Possible_Type
              (Minerva.Primitives.Standard_Boolean);
            Element.Condition.Check (Environment);
         end if;
         Element.Sequence.Check (Environment);
      end loop;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children
     (This : Instance)
      return Class_Reference_Array
   is
      function Get_Children
        (Current : Element_Lists.Cursor)
         return Class_Reference_Array;

      ------------------
      -- Get_Children --
      ------------------

      function Get_Children
        (Current : Element_Lists.Cursor)
         return Class_Reference_Array
      is
      begin
         if Element_Lists.Has_Element (Current) then
            declare
               subtype Tree_Reference is
                 Minerva.Trees.Class_Reference;
               use type Minerva.Trees.Expressions.Class_Reference;
               Element : constant Element_Type :=
                           Element_Lists.Element (Current);
            begin
               return (if Element.Condition = null
                       then Null_Class_Reference_Array
                       else (1 => Tree_Reference (Element.Condition)))
                 & (1 => Tree_Reference (Element.Sequence))
                 & Get_Children (Element_Lists.Next (Current));
            end;
         else
            return Null_Class_Reference_Array;
         end if;
      end Get_Children;

   begin
      return Get_Children (This.Elements.First);
   end Children;

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Code.Instance)
   is
      use type Minerva.Trees.Expressions.Class_Reference;
      Have_Next_Label : Boolean := False;
      Next_Label : Tagatha.Labels.Label;
      Out_Label  : constant Tagatha.Labels.Label := Unit.Next_Label;
   begin
      for Element of This.Elements loop
         if Have_Next_Label then
            Unit.Label (Next_Label);
            Have_Next_Label := False;
         end if;
         if Element.Condition /= null then
            Next_Label := Unit.Next_Label;
            Have_Next_Label := True;
            Element.Condition.Push (Unit);
            Unit.Branch (Tagatha.C_Equal, Next_Label);
         end if;
         Element.Sequence.Compile (Unit);
         Unit.Branch (Tagatha.C_Always, Out_Label);
      end loop;
      if Have_Next_Label then
         Unit.Label (Next_Label);
      end if;
      Unit.Label (Out_Label);
   end Compile_Tree;

   ------------
   -- Create --
   ------------

   function Create
     (Position : GCS.Positions.File_Position)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Elements => Element_Lists.Empty_List)
      do
         Result.Initialize (Position);
      end return;
   end Create;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return "if ...";
   end Image;

end Minerva.Trees.Statements.If_Statement;
