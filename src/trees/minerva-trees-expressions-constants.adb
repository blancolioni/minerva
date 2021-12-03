with Tagatha.Constants;
with Tagatha.Transfers;

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

   --------------------
   -- Constrain_Type --
   --------------------

   overriding procedure Constrain_Type
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List)
   is
      Universal : constant Minerva.Types.Class_Reference :=
                    This.Available_Types.First_Element;
   begin
      This.Available_Types.Clear;
      for Possible of Possible_Types loop
         if Universal.Is_Convertible_To (Possible) then
            This.Available_Types.Append (Possible);
         end if;
      end loop;
   end Constrain_Type;

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

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error with
        "cannot pop constant " & Image (Dispatch (This));
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
      procedure Push (Const : Tagatha.Constants.Tagatha_Constant);

      procedure Push_Integer (Value : Tagatha.Tagatha_Integer);
      procedure Push_Float (Value : Tagatha.Tagatha_Floating_Point);

      ----------
      -- Push --
      ----------

      procedure Push (Const : Tagatha.Constants.Tagatha_Constant)
      is
      begin
         Unit.Push_Operand
           (Op   =>
              Tagatha.Transfers.Constant_Operand
                (Const,
                 Size =>
                   Tagatha.Bits_To_Size (This.Get_Type.Size_Bits)));
      end Push;

      ----------------
      -- Push_Float --
      ----------------

      procedure Push_Float (Value : Tagatha.Tagatha_Floating_Point) is
         Op : constant Tagatha.Constants.Tagatha_Constant :=
                Tagatha.Constants.Floating_Point_Constant (Value);
      begin
         Push (Op);
      end Push_Float;

      ------------------
      -- Push_Integer --
      ------------------

      procedure Push_Integer (Value : Tagatha.Tagatha_Integer) is
         Op : constant Tagatha.Constants.Tagatha_Constant :=
                Tagatha.Constants.Integer_Constant (Value);
      begin
         Push (Op);
      end Push_Integer;

   begin
      case This.Const_Type is
         when Universal_Integer_Constant =>
            Push_Integer
              (Tagatha.Tagatha_Integer'Value
                 (Minerva.Names.Standard_Text (This.Image)));
         when Universal_Float_Constant =>
            Push_Float
              (Tagatha.Tagatha_Floating_Point'Value
                 (Minerva.Names.Standard_Text (This.Image)));
         when Universal_String_Constant =>
            null;
      end case;
   end Push;

   -------------------------
   -- Set_Available_Types --
   -------------------------

   overriding procedure Set_Available_Types
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id)
   is
   begin
      case This.Const_Type is
         when Universal_Integer_Constant =>
            This.Available_Types.Append
              (Minerva.Types.Universal.Universal_Integer);
         when Universal_Float_Constant =>
            This.Available_Types.Append
              (Minerva.Types.Universal.Universal_Float);
         when Universal_String_Constant =>
            null;
      end case;
   end Set_Available_Types;

end Minerva.Trees.Expressions.Constants;
