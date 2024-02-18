with Minerva.Entries.Value.Components;
with Minerva.Entries.Withs;
with Minerva.Names;
with Minerva.Types.Composite;

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
      Id_Types   : Minerva.Types.Lists.List;
   begin
      if This.Left.Has_Entry then
         if This.Left.Get_Entry.Is_Package_Reference then
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
                      ". only implemented for packages");
         end if;
      else
         for Left_Type of Left_Types loop
            if Left_Type.Is_Composite then
               declare
                  Composite : constant Minerva.Types.Composite.Class_Reference
                    := Minerva.Types.Composite.To_Composite (Left_Type);
                  Env       : constant Minerva.Environment.Environment_Id :=
                                Composite.Component_Environment;
               begin
                  if Minerva.Environment.Exists (Env, This.Right) then
                     declare
                        Component : constant Entries.Constant_Class_Reference
                          := Minerva.Environment.Get
                            (Env, This.Right);
                     begin
                        This.Matching_Entries.Append (Component);
                        Id_Types.Append (Component.Entry_Type);
                     end;
                  end if;
               end;
            end if;
         end loop;
      end if;

      if Id_Types.Is_Empty then
         This.Add_Error ("invalid-prefix-for-identifier,"
                         & Minerva.Names.Cased_Text (This.Right));
      end if;
      return Id_Types;
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

   -----------------
   -- Get_Partial --
   -----------------

   overriding function Get_Partial
     (This : Instance)
      return Minerva.Partials.Reference
   is
      Dot_Entry : constant Minerva.Entries.Constant_Class_Reference :=
                    This.Get_Entry;
   begin
      if Dot_Entry.all in Minerva.Entries.Value.Components.Instance'Class then
         declare
            subtype Component_Reference is
              Minerva.Entries.Value.Components.Constant_Class_Reference;
            Component : constant Component_Reference :=
                          Component_Reference (This.Get_Entry);
         begin
            return This.Left.Get_Partial.Add_Offset (Component.Word_Offset);
         end;
      else
         return Dot_Entry.Get_Partial;
      end if;
   end Get_Partial;

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
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      This.Get_Partial.Pop (Code);
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      This.Get_Partial.Push (Code);
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      This.Left.Push_Address (Code);
      This.Get_Entry.Push_Address (Code);
   end Push_Address;

   --------------
   -- Set_Type --
   --------------

   overriding procedure Set_Type
     (This          : in out Instance;
      Possible_Type : not null Minerva.Types.Class_Reference)
   is
      subtype Entry_Reference is
        Entries.Constant_Class_Reference;
      Found_Matching_Entry : Boolean := False;
      Found_Left_Entry     : Boolean := False;
      Left_Entry : Minerva.Entries.Constant_Class_Reference;
   begin
      This.Log ("setting type to "
                & Possible_Type.Short_Name);

      for Matching_Entry of This.Matching_Entries loop

         if Matching_Entry.Entry_Type.Is_Convertible_To (Possible_Type) then
            pragma Assert (not Found_Matching_Entry,
                           "multiple matches in set type");
            This.Set_Entry (Matching_Entry);
            This.Expression_Type := Possible_Type;
            Found_Matching_Entry := True;
         end if;
      end loop;

      pragma Assert (Found_Matching_Entry, "no matching types in set type");

      if not This.Left.Has_Entry then
         for Matching_Entry of
           This.Left.Matching_Entries
         loop
            declare
               Left_Type : constant Minerva.Types.Class_Reference :=
                             Matching_Entry.Entry_Type;
            begin
               if Left_Type.Is_Composite then
                  declare
                     Composite : constant Types.Composite.Class_Reference
                       := Types.Composite.To_Composite (Left_Type);
                     Env       : constant Minerva.Environment.Environment_Id :=
                                   Composite.Component_Environment;
                  begin
                     if Minerva.Environment.Exists (Env, This.Right) then
                        declare
                           use type Entries.Constant_Class_Reference;
                           Component : constant Entry_Reference :=
                                         Minerva.Environment.Get
                                           (Env, This.Right);
                        begin
                           if Component = This.Get_Entry then
                              if Found_Left_Entry then
                                 This.Add_Error ("ambiguous-prefix");
                              else
                                 Left_Entry := Matching_Entry;
                                 Found_Left_Entry := True;
                              end if;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;

         if Found_Left_Entry then
            This.Left.Set_Type (Left_Entry.Entry_Type);
         else
            raise Constraint_Error with
            This.Image & ": no matching entry";
         end if;
      end if;

   end Set_Type;

end Minerva.Trees.Expressions.Dots;
