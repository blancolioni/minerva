with Minerva.Entries.Withs;
with Minerva.Names;

package body Minerva.Trees.Expressions.Dots is

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return Maybe (This.Left);
   end Children;

   ---------------------
   -- Constrain_Types --
   ---------------------

   overriding function Constrain_Types
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
      return Minerva.Types.Lists.List
   is
      Left_Types : constant Minerva.Types.Lists.List :=
                     This.Left.Constrain_Types
                       (Minerva.Types.Lists.Empty_List, Environment);
      pragma Unreferenced (Left_Types);
   begin
      if This.Left.Has_Entry
        and then This.Left.Get_Entry.Is_Package_Reference
      then
         declare
            Package_Env : constant Minerva.Ids.Environment_Id :=
                            Minerva.Entries.Withs.Constant_Class_Reference
                              (This.Left.Get_Entry)
                              .Child_Environment;
         begin
            return This.Constrain_Identifier
              (This.Right, Possible_Types, Package_Env);
         end;
      else
         return (raise Constraint_Error with
                   ". only implmented for packages");
      end if;
   end Constrain_Types;

   ---------------------------
   -- Create_Dot_Expression --
   ---------------------------

   function Create_Dot_Expression
     (Position : GCS.Positions.File_Position;
      Left     : Expressions.Class_Reference;
      Right    : String)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
           Left  => Left,
           Right => Minerva.Names.To_Name (Right))
      do
         Result.Initialize (Position);
      end return;
   end Create_Dot_Expression;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Minerva.Values.Minerva_Value
   is
   begin
      return (raise Constraint_Error with
                "unimplemented: evaluate (dot)");
   end Evaluate;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return This.Left.Image & "." & Minerva.Names.Cased_Text (This.Right);
   end Image;

   ---------------
   -- Is_Static --
   ---------------

   overriding function Is_Static
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Boolean
   is
   begin
      return False;
   end Is_Static;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      null;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      if This.Left.Has_Entry
        and then This.Left.Get_Entry.Is_Package_Reference
      then
         This.Get_Entry.Push (Unit);
      end if;
   end Push;

   --------------
   -- Set_Type --
   --------------

   overriding procedure Set_Type
     (This          : in out Instance;
      Possible_Type : Minerva.Types.Class_Reference)
   is
      Found : Boolean := False;
   begin
      This.Log ("setting type to "
                & Possible_Type.Short_Name);

      for Matching_Entry of This.Matching_Entries loop

         if Matching_Entry.Entry_Type.Is_Convertible_To (Possible_Type) then
            pragma Assert (not Found, "multiple matches in set type");
            This.Set_Entry (Matching_Entry);
            This.Expression_Type := Possible_Type;
            Found := True;
         end if;
      end loop;
      pragma Assert (Found, "no matching types in set type");
   end Set_Type;

end Minerva.Trees.Expressions.Dots;
