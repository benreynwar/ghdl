--  GHDL Run Time (GRT) -  Run Time Information Instances.
--  Copyright (C) 2020 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Grt.Errors; use Grt.Errors;

package body Grt.Rtiis is
   
   function "&" (Left : Ghdl_C_String; Right : String) return Ghdl_C_String is
      New_String : String := Left.all;
   begin
      New_String := New_String & Right & NUL;
      return To_Ghdl_C_String_Assuming_Null(New_String);
   end "&";

   function "&" (Left : Ghdl_C_String; Right : Ghdl_C_String)
                return Ghdl_C_String is
      New_String : String := Left.all;
   begin
      New_String := New_String & Right.all & NUL;
      return To_Ghdl_C_String_Assuming_Null(New_String);
   end "&";

   function "&" (Left : String; Right : Ghdl_C_String) return Ghdl_C_String is
      New_String : String := Left;
   begin
      New_String := New_String & Right.all & NUL;
      return To_Ghdl_C_String_Assuming_Null(New_String);
   end "&";


   -- Find the layout information address for a Rti type given it's context.
   -- For types that have no layout information a null address is returned.
   function Get_Type_Layout(Rti : Ghdl_Rti_Access; Ctxt : Rti_Context)
                           return Address is
      Layout_Addr : Address;
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               St : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
                 To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti);
            begin
               Layout_Addr := Loc_To_Addr (
                  St.Common.Depth, St.Range_Loc, Ctxt);
            end;
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B1 =>
            Layout_Addr := Null_Address;
         when Ghdl_Rtik_Type_Array =>
            -- We can't have an unbounded type as the port/signal.
            -- and there's nowhere for the Rtin_Obj to hold the bounds.
            Internal_Error("to_ghdl_rtii");
         when Ghdl_Rtik_Subtype_Array =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
            begin
               Layout_Addr := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
            end;
         when Ghdl_Rtik_Type_Record =>
            declare
               Bt : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Rti);
            begin
               if Rti_Complex_Type (Rti) then
                  Layout_Addr := Loc_To_Addr (
                     Bt.Common.Depth, Bt.Layout, Ctxt);
               else
                  -- FIXME:  Is this right?
                  --Layout_Addr := Bt.Layout;
                  null;
               end if;
            end;
         when Ghdl_Rtik_Subtype_Record =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
            begin
               Layout_Addr := Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
            end;
         when others =>
            Internal_Error("to_ghdl_rtii");
      end case;
      return Layout_Addr;
   end Get_Type_Layout;

   function Get_Array_Nbr_Children(Rtii : Ghdl_Rtii) return Ghdl_Index_Type is
      Kind : Ghdl_Rtik;
      Base_Array : Ghdl_Rtin_Type_Array_Acc;
      Layout_Addr : Address;
      Bounds_Addr : Address;
      Length : Ghdl_Index_Type;
      Idx_Rti : Ghdl_Rti_Access;
      Rng : Ghdl_Range_Ptr;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Array =>
            Base_Array := To_Ghdl_Rtin_Type_Array_Acc(Rtii.Typ.Rti);
            Layout_Addr := Rtii.Typ.Binding_Layout_Addr;
         when Ghdl_Rtik_Subtype_Array =>
            Base_Array := To_Ghdl_Rtin_Type_Array_Acc(
               To_Ghdl_Rtin_Subtype_Composite_Acc(Rtii.Typ.Rti).Basetype);
            Layout_Addr := Rtii.Typ.Layout_Addr;
         when others =>
            Internal_Error("get_array_element_type");
      end case;
      Bounds_Addr := Layout_Addr + Ghdl_Index_Type'(
         Ghdl_Indexes_Type'Size / 8);
      -- Get the length of this dimension so we can check whether
      -- the index is valid.
      Idx_Rti := Get_Base_Type (Base_Array.Indexes (Rtii.Typ.Dim));
      Extract_Range (Bounds_Addr, Idx_Rti, Rng);
      Length := Range_To_Length (Rng, Idx_Rti);
      return Length;
   end Get_Array_Nbr_Children;

   function Get_Array_Child(Rtii : Ghdl_Rtii; Index : Ghdl_Index_Type)
                           return Ghdl_Rtii is
      Kind : Ghdl_Rtik;
      Base_Array : Ghdl_Rtin_Type_Array_Acc;
      El_Type : Ghdl_Rtii_Type;
      Layout_Addr : Address;
      Binding_Layout_Addr : Address;
      Bounds_Addr : Address;
      Child : Ghdl_Rtii;
      Length : Ghdl_Index_Type;
      Idx_Rti : Ghdl_Rti_Access;
      Rng : Ghdl_Range_Ptr;
      El_Layout_Addr : Address;
      El_In_Step : Ghdl_Index_Type;
      Child_Addr : Address;
      Sizes : Ghdl_Indexes_Ptr;
      El_Size : Ghdl_Index_Type;
      El_Name : Ghdl_C_String;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Array =>
            Base_Array := To_Ghdl_Rtin_Type_Array_Acc(Rtii.Typ.Rti);
            Layout_Addr := Rtii.Typ.Binding_Layout_Addr;
         when Ghdl_Rtik_Subtype_Array =>
            Base_Array := To_Ghdl_Rtin_Type_Array_Acc(
               To_Ghdl_Rtin_Subtype_Composite_Acc(Rtii.Typ.Rti).Basetype);
            Layout_Addr := Rtii.Typ.Layout_Addr;
         when others =>
            Internal_Error("get_array_child");
      end case;
      Sizes := To_Ghdl_Indexes_Ptr(Layout_Addr);
      if Rtii.Is_Sig then
         El_Size := Sizes.Signal;
      else
         El_Size := Sizes.Value;
      end if;
      Bounds_Addr := Layout_Addr + Ghdl_Index_Type'(
                        Ghdl_Indexes_Type'Size / 8);
      -- Get the number of elements contained in one
      -- index increment of this dimension.
      El_In_Step := 1;
      for Dim in Rtii.Typ.Dim+1 .. Base_Array.Nbr_Dim-1 loop
         Idx_Rti := Get_Base_Type (Base_Array.Indexes (Dim));
         Extract_Range (Bounds_Addr, Idx_Rti, Rng);
         Length := Range_To_Length (Rng, Idx_Rti);
         El_In_Step := El_In_Step * Length;
      end loop;
      -- Get the length of this dimension so we can check whether
      -- the index is valid.
      Idx_Rti := Get_Base_Type (Base_Array.Indexes (Rtii.Typ.Dim));
      Extract_Range (Bounds_Addr, Idx_Rti, Rng);
      Length := Range_To_Length (Rng, Idx_Rti);
      El_Name := Rtii.Name & "[" & Ghdl_Index_Type'Image(Index) & "]";
      -- Work out what the address is of the child.
      if Index >= Length then
         Internal_Error("get_array_child");
      else
         Child_Addr := Rtii.Addr + El_Size * Index * El_In_Step;
      end if;
      if Rtii.Typ.Dim = Base_Array.Nbr_Dim-1 then
         El_Layout_Addr := Get_Type_Layout(
            To_Ghdl_Rtin_Element_Acc(Base_Array.Element).Eltype,
            Rtii.Typ.Ctxt);
         -- There is no way for an array type to pass bounds to it's element.
         -- This is implemented with an anonymous subtype so we don't
         --  need to worry about passing the bounds here.
         Binding_Layout_Addr := Null_Address;
         -- The child is one of the array elements.
         El_Type := Ghdl_Rtii_Type'(
            Rti => Base_Array.Element,
            Ctxt => Rtii.Typ.Ctxt,
            Layout_Addr => El_Layout_Addr,
            Binding_Layout_Addr => Binding_Layout_Addr,
            Dim => 0);
         Child := Ghdl_Rtii'(
            Is_Sig => Rtii.Is_Sig,
            Typ => El_Type,
            Name => El_Name,
            Base_Rti => Rtii.Base_Rti,
            Addr => Child_Addr);
      else
         -- The child is a subset of this array with one less
         --  dimension.
         Child := Ghdl_Rtii'(
            Is_Sig => Rtii.Is_Sig,
            Typ => Rtii.Typ,
            Name => El_Name,
            Base_Rti => Rtii.Base_Rti,
            Addr => Child_Addr);
      end if;
      return Child;
   end Get_Array_Child;

   function Get_Record_Nbr_Children(Rtii : Ghdl_Rtii)
                                   return Ghdl_Index_Type is
      Kind : Ghdl_Rtik;
      Base_Record : Ghdl_Rtin_Type_Record_Acc;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Record =>
            Base_Record := To_Ghdl_Rtin_Type_Record_Acc(Rtii.Typ.Rti);
         when Ghdl_Rtik_Subtype_Array =>
            Base_Record := To_Ghdl_Rtin_Type_Record_Acc(
               To_Ghdl_Rtin_Subtype_Composite_Acc(Rtii.Typ.Rti).Basetype);
         when others =>
            Internal_Error("get_record_element_type");
      end case;
      return Base_Record.Nbrel;
   end Get_Record_Nbr_Children;

   function Get_Record_Child(Rtii : Ghdl_Rtii; Index : Ghdl_Index_Type)
                             return Ghdl_Rtii is
      Kind : Ghdl_Rtik;
      Base_Record : Ghdl_Rtin_Type_Record_Acc;
      El_Type : Ghdl_Rtii_Type;
      Child_Addr : Address;
      El_Rti : Ghdl_Rtin_Element_Acc;
      El_Layout_Addr : Address;
      Type_Layout_Addr : Address;
      Child : Ghdl_Rtii;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Record =>
            Base_Record := To_Ghdl_Rtin_Type_Record_Acc(Rtii.Typ.Rti);
         when Ghdl_Rtik_Subtype_Array =>
            Base_Record := To_Ghdl_Rtin_Type_Record_Acc(
               To_Ghdl_Rtin_Subtype_Composite_Acc(Rtii.Typ.Rti).Basetype);
         when others =>
            Internal_Error("get_record_child");
      end case;
      if Index >= Base_Record.Nbrel then
         Internal_Error("get_record_child");
      else
         El_Rti := To_Ghdl_Rtin_Element_Acc(Base_Record.Elements(Index));
         El_Layout_Addr := Rtii.Typ.Layout_Addr + El_Rti.Layout_Off;
         Type_Layout_Addr := Get_Type_Layout(El_Rti.Eltype, Rtii.Typ.Ctxt);
         El_Type := Ghdl_Rtii_Type'(
            Rti => El_Rti.Eltype,
            Ctxt => Rtii.Typ.Ctxt,
            Layout_Addr => Type_Layout_Addr,
            Binding_Layout_Addr => El_Layout_Addr,
            Dim => 0);
         El_Name := Rtii.Name & "." & El_Rti.Name;
         -- FIXME : We need to deal with complex and simple layouts.
         if Rtii.Is_Sig then
            Child_Addr := Rtii.Addr + El_Rti.Sig_Off;
         else
            Child_Addr := Rtii.Addr + El_Rti.Val_Off;
         end if;
         Child := Ghdl_Rtii'(
            Is_Sig => Rtii.Is_Sig,
            Typ => El_Type,
            Name => El_Name,
            Base_Rti => Rtii.Base_Rti,
            Addr => Child_Addr);
      end if;
      return Child;
   end Get_Record_Child;

   -- Get an indexed child of an Rtii.
   function Get_Rtii_Child(Rtii : Ghdl_Rtii;
                           Index : Ghdl_Index_Type)
                          return Ghdl_Rtii is
      Kind : Ghdl_Rtik;
      Child : Ghdl_Rtii;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Record |
              Ghdl_Rtik_Subtype_Record =>
            Child := Get_Record_Child(Rtii, Index);
         when Ghdl_Rtik_Subtype_Array |
              Ghdl_Rtik_Type_Array =>
            Child := Get_Array_Child(Rtii, Index);
         when others =>
            Internal_Error("get_rtii_child");
      end case;
      return Child;
   end Get_Rtii_Child;

   -- Get number of children in a Rtii.
   function Get_Rtii_Nbr_Children(Rtii : Ghdl_Rtii) return Ghdl_Index_Type is
      Kind : Ghdl_Rtik;
      Nbr_Children : Ghdl_Index_Type;
   begin
      Kind := Rtii.Typ.Rti.Kind;
      case Kind is
         when Ghdl_Rtik_Type_Record |
              Ghdl_Rtik_Subtype_Record =>
            Nbr_Children := Get_Record_Nbr_Children(Rtii);
         when Ghdl_Rtik_Subtype_Array |
              Ghdl_Rtik_Type_Array =>
            Nbr_Children := Get_Array_Nbr_Children(Rtii);
         when others =>
            Internal_Error("get_rtii_nbr_children");
      end case;
      return Nbr_Children;

   end Get_Rtii_Nbr_Children;

   function To_Ghdl_Rtii(Rti : Ghdl_Rtin_Object_Acc; Ctxt : Rti_Context)
                        return Ghdl_Rtii is
      Is_Sig : Boolean;
      Addr : Address;
      Layout_Addr : Address;
      Typ : Ghdl_Rtii_Type;
      Rtii : Ghdl_Rtii;
   begin
      case Rti.Common.Kind is
         when Ghdl_Rtik_Port |
              Ghdl_Rtik_Signal =>
            Is_Sig := True;
         when Ghdl_Rtik_Constant |
              Ghdl_Rtik_Generic =>
            Is_Sig := False;
         when others =>
            Internal_Error("to_ghdl_rtii");
      end case;
      Addr := Loc_To_Addr(Depth => Rti.Common.Depth,
                          Loc => Rti.Loc,
                          Ctxt => Ctxt);
      Layout_Addr := Get_Type_Layout(Rti.Obj_Type, Ctxt);
      Typ := (
         Rti => Rti.Obj_Type,
         Ctxt => Ctxt,
         Layout_Addr => Layout_Addr,
         -- At the top level we don't try to deal with
         -- any unbound types.
         Binding_Layout_Addr => Null_Address,
         Dim => 0);
      Rtii := (
         Is_Sig => Is_Sig,
         Base_Rti => Rti,
         Typ => Typ,
         Name => Rti.Name,
         Addr => Addr);
      return Rtii;
   end To_Ghdl_Rtii;
   
   function Is_Record(Rtii : Ghdl_Rtii) return Boolean is
   begin
      case Rtii.Typ.Rti.Kind is
         when Ghdl_Rtin_Type_Record
           | Ghdl_Rtin_Subtype_Record
           | Ghdl_Rtin_Type_Unbounded_Record
           | Ghdl_Rtin_Subtype_Unbounded_Record =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Record;

   function Is_Array(Rtii : Ghdl_Rtii) return Boolean is
   begin
      case Rtii.Typ.Rti.Kind is
         when Ghdl_Rtin_Type_Array
           | Ghdl_Rtin_Subtype_Array =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Array;

   function Get_Bit_Size (Rtii : Ghdl_Rtii) return Ghdl_Index_Type is
   begin
      case Rtii.Typ.Rti.Kind is
         -- For records we sum the bit sizes of all the elements.
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Subtype_Record =>
            declare
               Size : Ghdl_Index_Type := 0;
               N_Els : Ghdl_Index_Type;
               Child : Ghdl_Rtii;
            begin
               N_Els := Get_Size(Rtii);
               for El_Index in 0 .. N_Els-1 loop
                  Child := Get_Child(Rtii, El_Index);
                  Size := Size + Get_Bit_Size(Child);
               end loop;
            end;
         -- For arrays we find the bit size of a single element and then
         -- multiply by the number of elements.
         when Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Array =>
            declare
               Size : Ghdl_Index_Type;
               Child : Ghdl_Rtii;
            begin
              Size := Get_Size(Rtii);
              if Size = 0 then
                 return 0;
              else
                 Child := Get_Chlid(Rtii, 0);
                 Child_Bit_Size := Get_Bit_Size(Child);
                 return Size * Child_Bit_Size;
              end if;
            end;
         when others =>
            return Get_Size(Rtii);
      end case;
   end Get_Bit_Size;

   function Get_Size (Rtii : Ghdl_Rtii) return Ghdl_Index_Type is
   begin
      case Rtii.Typ.Rti.Kind is
         -- For records and arrays the size is the number of elements.
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Subtype_Record 
           | Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Array =>
            return Get_Number_Children(Rtii);
         -- Otherwise it's just the number of bits used.
         when Ghdl_Rtik_Type_B1 =>
            return 1;
         when Ghdl_Rtik_Type_E8 =>
            return 8;
         when Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_P32
           | Ghdl_Rtik_Type_I32 =>
            return 32;
         when Ghdl_Rtik_Type_I64
           | Ghdl_Rtik_Type_F64
           | Ghdl_Rtik_Type_P64 =>
            return 64;
         when Vcd_Bool
           | Vcd_Bit
           | Vcd_Stdlogic =>
            return 1;
         when others =>
            Internal_Error("get_size: unknown type kind");
      end case;
   end Get_Size;

   function Ranges_To_Length (Rngs : Ghdl_Range_Array;
                              Indexes : Ghdl_Rti_Arr_Acc)
                             return Ghdl_Index_Type
   is
      Res : Ghdl_Index_Type;
   begin
      Res := 1;
      for I in Rngs'Range loop
         Res := Res * Range_To_Length
           (Rngs (I), Get_Base_Type (Indexes (I - Rngs'First)));
      end loop;
      return Res;
   end Ranges_To_Length;

end Grt.Rtiis;
