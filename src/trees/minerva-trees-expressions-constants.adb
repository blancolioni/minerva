with Tagatha.Operands;

with Minerva.Names;
with Minerva.Types;

with Minerva.Types.Universal;

package body Minerva.Trees.Expressions.Constants is

   --------------
   -- Children --
   --------------

   overriding function Children
     (This : Instance)
      return Class_Reference_Array
   is
      Result : Class_Reference_Array (1 .. 0);
   begin
      return Result;
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
   begin
      return Result : Minerva.Types.Lists.List do
         for Possible of Possible_Types loop
            if This.Universal_Type.Is_Convertible_To (Possible) then
               Result.Append (Possible);
            end if;
         end loop;
      end return;
   end Constrain_Types;

   ----------------------------
   -- Create_Universal_Float --
   ----------------------------

   function Create_Universal_Float
     (Position : GCS.Positions.File_Position;
      Image    : String)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize (Position);
         Result.Const_Type := Universal_Float_Constant;
         Result.Image := Minerva.Names.To_Name (Image);
         Result.Universal_Type :=
           Minerva.Types.Universal.Universal_Float;
      end return;
   end Create_Universal_Float;

   ------------------------------
   -- Create_Universal_Integer --
   ------------------------------

   function Create_Universal_Integer
     (Position : GCS.Positions.File_Position;
      Image    : String)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize (Position);
         Result.Const_Type := Universal_Integer_Constant;
         Result.Image := Minerva.Names.To_Name (Image);
         Result.Universal_Type :=
           Minerva.Types.Universal.Universal_Integer;
      end return;
   end Create_Universal_Integer;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Minerva.Values.Minerva_Value
   is
      Img : constant String := Image (Dispatch (This));
   begin
      case This.Const_Type is
         when Universal_Integer_Constant =>
            return Minerva.Values.Signed_Integer_Value
              (Value_Type => This.Get_Type,
               Value      => Integer'Value (Img));
         when Universal_Float_Constant =>
            return Minerva.Values.Floating_Point_Value
              (Value_Type => This.Get_Type,
               Value      => Float'Value (Img));
         when Universal_String_Constant =>
            return (raise Constraint_Error
                      with "constant string values not implemented");
      end case;
   end Evaluate;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return Minerva.Names.Cased_Text (This.Image);
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
      return True;
   end Is_Static;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Code.Instance)
   is

      procedure Push (Operand : Tagatha.Operands.Operand_Type);

      ----------
      -- Push --
      ----------

      procedure Push (Operand : Tagatha.Operands.Operand_Type) is
      begin
         Unit.Push
           (Tagatha.Operands.Set_Size
              (This.Get_Type.Size,
               Operand));
      end Push;

   begin
      case This.Const_Type is
         when Universal_Integer_Constant =>
            Push
              (Tagatha.Operands.Constant_Operand
                 (Tagatha.Tagatha_Integer'Value
                      (Minerva.Names.Standard_Text (This.Image))));
         when Universal_Float_Constant =>
            Push
              (Tagatha.Operands.Set_Data_Type
                 (Tagatha.Floating_Point_Data,
                  Tagatha.Operands.Constant_Operand
                    (Tagatha.Tagatha_Floating_Point'Value
                         (Minerva.Names.Standard_Text (This.Image)))));
         when Universal_String_Constant =>
            null;
      end case;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Code.Instance)
   is
   begin
      raise Constraint_Error with
        "cannot push address of constant " & Image (Dispatch (This));
   end Push_Address;

end Minerva.Trees.Expressions.Constants;
