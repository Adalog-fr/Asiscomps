----------------------------------------------------------------------
--  Thick_Queries - Package body                                    --
--  Copyright (C) 2002-2009 Adalog                                  --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------
pragma Ada_05;
--## Rule off Use_Img_Function ## This package should not depend on Utilities
with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Maps.Wide_Constants;

with   -- Reusable components
  A4G_Bugs;

with   -- ASIS units
  Asis.Clauses,
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Errors,
  Asis.Exceptions,
  Asis.Expressions,
  Asis.Implementation,
  Asis.Iterator,
  Asis.Limited_Views,
  Asis.Statements,
  Asis.Text;
package body Thick_Queries is
   use Asis, Asis.Elements, Asis.Declarations, Asis.Statements;

   ------------------------------------------------------------------
   -- Internal utilities                                           --
   ------------------------------------------------------------------

   ----------------
   -- Impossible --
   ----------------

   User_Error_Proc : Error_Procedure := null;

   procedure Impossible (Message : Wide_String; E : Asis.Element);
   pragma No_Return (Impossible);
   procedure Impossible (Message : Wide_String; E : Asis.Element) is
      use Ada.Exceptions, Ada.Characters.Handling, Asis.Text;
      S : constant Span := Element_Span (E);
   begin
      if User_Error_Proc /= null then
         User_Error_Proc (Message, E);
         -- Normally, the call to the user proc should raise an exception.
         -- If it doesn't, we'll continue with the code below, so we
         -- are certain that an exception will be raised in any case.
      end if;

      Raise_Exception (Program_Error'Identity,
                       Message => To_String (Message) &
                         " at" & Line_Number'Image (S.First_Line) &
                         ":"   & Character_Position'Image (S.First_Column));
   end Impossible;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (S : Wide_String) return Wide_String is
      use Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Maps.Wide_Constants;
   begin
      return Translate (S, Upper_Case_Map);
   end To_Upper;

   ------------------------------------------------------------------
   -- Exported subprograms                                         --
   ------------------------------------------------------------------

   -------------------------
   -- Set_Error_Procedure --
   -------------------------

   procedure Set_Error_Procedure (To : Error_Procedure) is
   begin
      User_Error_Proc := To;
   end Set_Error_Procedure;

   ---------------------
   -- Biggest_Int_Img --
   ---------------------

   function Biggest_Int_Img (Item : Biggest_Int) return Wide_String is
      Result : constant Wide_String := Biggest_Int'Wide_Image (Item); --## Rule line OFF Use_Img_Function
      subtype Slide is Wide_String (1 .. Result'Length-1);
   begin
      if Item < 0 then
         return Result;
      else
         return Slide (Result (2 .. Result'Last));
      end if;
   end Biggest_Int_Img;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Extended_Biggest_Int_List) return Boolean is
      R : Asis.List_Index;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      R := Right'First;
      for L in Left'Range loop
         if Left (L) = Not_Static or else Right (R) = Not_Static or else Left (L) /= Right (R) then
            return  False;
         end if;
         R := R + 1;
      end loop;
      return True;
   end "=";

   ---------------------------
   -- Access_Target_Type --
   ---------------------------

   function Access_Target_Type (The_Subtype : Asis.Element) return Asis.Declaration is
      use Asis.Definitions, Asis.Expressions;

      Good_Def : Asis.Definition;
      Decl     : Asis.Declaration;
   begin  -- Access_Target_Type
      case Element_Kind (The_Subtype) is
         when A_Defining_Name =>
              Good_Def := Type_Declaration_View (Enclosing_Element (The_Subtype));
         when A_Definition =>
            Good_Def := The_Subtype;
         when A_Declaration =>
            Good_Def := Type_Declaration_View (The_Subtype);
         when others =>
            Impossible ("Inappropriate element kind in Access_Target_Subtype", The_Subtype);
      end case;

      case Definition_Kind (Good_Def) is
         when An_Access_Definition => -- ASIS 2005
            -- No declaration here, but cannot be a derived type
            -- But can be a private or incomplete type...
            Decl := Corresponding_Name_Declaration
                    (Simple_Name
                     (Strip_Attributes
                      (Anonymous_Access_To_Object_Subtype_Mark (Good_Def))));
            return Corresponding_First_Subtype (Corresponding_Full_Type_Declaration (Decl));
         when A_Type_Definition =>
            if Type_Kind (Good_Def) = An_Access_Type_Definition then
               Good_Def := Type_Declaration_View (Ultimate_Type_Declaration (Enclosing_Element (Good_Def)));
               if Access_Type_Kind (Good_Def) not in Asis.Access_To_Object_Definition then
                  return Nil_Element;
               end if;
               Decl := Corresponding_Name_Declaration
                        (Simple_Name
                         (Strip_Attributes
                          (Subtype_Simple_Name
                           (Asis.Definitions.Access_To_Object_Definition (Good_Def)))));
               return  Corresponding_First_Subtype (Corresponding_Full_Type_Declaration (Decl));
            end if;
         when A_Formal_Type_Definition =>
            if Formal_Type_Kind (Good_Def) = A_Formal_Access_Type_Definition then
               Decl := Corresponding_Name_Declaration
                         (Simple_Name
                          (Strip_Attributes
                           (Subtype_Simple_Name
                            (Asis.Definitions.Access_To_Object_Definition
                             (Type_Declaration_View
                              (Ultimate_Type_Declaration
                               (Enclosing_Element (Good_Def))))))));
               return Corresponding_First_Subtype (Corresponding_Full_Type_Declaration (Decl));
            end if;
         when others =>
            null;
      end case;

      return Nil_Element;
   end Access_Target_Type;

   --------------------------
   -- Attribute_Name_Image --
   --------------------------

   function Attribute_Name_Image (Attribute : Asis.Expression) return Wide_String is
      use Asis.Expressions;
   begin
      return Name_Image (Attribute_Designator_Identifier (Attribute));
   end Attribute_Name_Image;

   ------------------------
   -- Called_Simple_Name --
   ------------------------

   function Called_Simple_Name (Call : Asis.Element) return Asis.Expression is
      use Asis.Expressions;
      Result : Asis.Expression;
   begin
      if Expression_Kind (Call) = A_Function_Call then
         Result := Prefix (Call);
      else
         -- Must be a procedure or entry call
         Result := Called_Name (Call);
         if Expression_Kind (Result) = An_Indexed_Component then
            -- A call to an entry family
            Result := Prefix (Result);
         end if;
      end if;

      if Expression_Kind (Result) = A_Selected_Component then
         Result := Selector (Result);
      end if;

      if Expression_Kind (Result) = An_Explicit_Dereference or else Is_Access_Expression (Result) then
         Result := Nil_Element;
      end if;

      return Result;
   end Called_Simple_Name;

   ------------------------------------
   -- Corresponding_Call_Description --
   ------------------------------------

   function Corresponding_Call_Description (Call : Asis.Element) return Call_Descriptor is
      use Asis.Expressions;
      Callee : Asis.Declaration;
      Name   : Asis.Expression;
   begin
      if Expression_Kind (Call) = A_Function_Call then
         Callee := Corresponding_Called_Function (Call);
      else
         Callee := Corresponding_Called_Entity (Call);
      end if;

      loop
         case Declaration_Kind (Callee) is
            when A_Procedure_Declaration
               | A_Function_Declaration
               | An_Expression_Function_Declaration   -- Ada 2012
               =>
               -- Callee might be a declaration whose body is provided by renaming.
               -- This must be handled as renaming
               if Declaration_Kind (Corresponding_Body (Callee))
                  not in A_Procedure_Renaming_Declaration .. A_Function_Renaming_Declaration
               then
                  -- Note that this includes Not_A_Declaration when there is
                  -- no body (like "/=" when "=" has been redefined)
                  exit;
               end if;
               Callee := Corresponding_Body (Callee);

            when A_Null_Procedure_Declaration =>   -- Ada 2005
               -- nothing to fear here
               return (Kind => A_Regular_Call, Declaration => Callee);

            when An_Enumeration_Literal_Specification =>
               return (Kind => An_Enumeration_Literal);

            when A_Renaming_Declaration =>
               Name := Simple_Name (Corresponding_Base_Entity (Callee));
               case Expression_Kind (Name) is
                  when An_Attribute_Reference =>
                     return (Kind => An_Attribute_Call);
                  when An_Explicit_Dereference =>
                     -- No possible implicit dereference here
                     return (Kind => A_Dereference_Call);
                  when An_Indexed_Component =>
                     -- Renaming of a member of an entry family
                     Callee := Corresponding_Name_Declaration (Simple_Name (Prefix (Name)));
                  when others =>
                     Callee := Corresponding_Name_Declaration (Name);
               end case;

            when others =>
               exit;
         end case;
      end loop;

      if not Is_Nil (Callee) then
         return (Kind => A_Regular_Call, Declaration => Callee);
      end if;

      if Is_Dispatching_Call (Call) then
         return (Kind => A_Dispatching_Call);
      end if;

      Name := Called_Simple_Name (Call);
      if Is_Nil (Name) then
         return (Kind => A_Dereference_Call);
      end if;

      if Expression_Kind (Name) = An_Attribute_Reference then
         return (Kind => An_Attribute_Call);
      end if;

      -- Short of being anything else, must be a predefined entity...
      return (Kind => A_Predefined_Entity_Call);
   end Corresponding_Call_Description;


   --------------------
   -- Called_Profile --
   --------------------

   function Called_Profile (Call : Asis.Element) return Asis.Parameter_Specification_List is
      use Asis.Expressions, Asis.Definitions;
      Callee : Asis.Declaration;
      Name   : Asis.Expression;
   begin
      if Expression_Kind (Call) = A_Function_Call then
         Callee := Corresponding_Called_Function (Call);
      else
         Callee := Corresponding_Called_Entity (Call);
      end if;

      if not Is_Nil (Callee) then
         if Declaration_Kind (Callee) in A_Procedure_Instantiation .. A_Function_Instantiation then
            Callee := Corresponding_Declaration (Callee);
         end if;

         return Parameter_Profile (Callee);
      end if;

      -- Called subprogram not statically known
      if Expression_Kind (Call) = A_Function_Call then
         Name := Prefix (Call);
      else
         Name := Called_Name (Call);
      end if;

      if Expression_Kind (Name) = An_Explicit_Dereference then
         Name := Prefix (Name);
      end if;

      if Expression_Kind (Name) = A_Selected_Component then
         Name := Selector (Name);
      end if;

      if Is_Access_Expression (Name) then
         -- access to subprogram (or entry)
         return Access_To_Subprogram_Parameter_Profile (Ultimate_Expression_Type (Name));
      end if;

      -- Dispatching Call, or call to a subprogram defined by an attribute (see Corresponding_Called_Entity
      -- in the ASIS specification).
      -- We have no way to get a profile. Sigh...
      return Nil_Element_List;
   end Called_Profile;

   -------------------
   -- Types_Profile --
   -------------------

   function Types_Profile (Declaration : in Asis.Element)  return Profile_Descriptor is
   -- Declaration is a callable entity declaration, or an anonymous access to subprogram definition

      function Build_Entry (Def : Asis.Element) return Profile_Entry is
      -- Def is the parameter or result type definition
         use Asis.Definitions, Asis.Expressions, Asis.Limited_Views;

         Good_Mark : Asis.Element;
         Attribute : Type_Attribute;
         Decl      : Asis.Declaration;
         Form      : constant Asis.Access_Definition_Kinds := Access_Definition_Kind (Def);
      begin
         case Form is
            when Not_An_Access_Definition =>
               -- Normal case
               null;
            when An_Anonymous_Access_To_Constant
               | An_Anonymous_Access_To_Variable
               =>
               declare
                  Ret_Val : Profile_Entry := Build_Entry (Anonymous_Access_To_Object_Subtype_Mark (Def));
               begin
                  Ret_Val.Access_Form := Form;
                  return Ret_Val;
               end;
            when An_Anonymous_Access_To_Procedure
               | An_Anonymous_Access_To_Protected_Procedure
               =>
               return (Access_Form  => Form,
                       Attribute    => None,
                       Name         => Nil_Element,
                       Anon_Profile => new Profile_Descriptor'(Types_Profile (Def)));
            when An_Anonymous_Access_To_Function
               | An_Anonymous_Access_To_Protected_Function
               =>
               return (Access_Form  => Form,
                       Attribute    => None,
                       Name         => Nil_Element,
                       Anon_Profile => new Profile_Descriptor'(Types_Profile (Def)));
         end case;

         -- Here, no more anonymous access types
         if Expression_Kind (Def) = An_Attribute_Reference then
            Good_Mark := Prefix (Def);

            case Attribute_Kind (Def) is
               when A_Base_Attribute =>
                  Attribute := Base;
               when A_Class_Attribute =>
                  Attribute := Class;
                  -- According to 3.9(14), T'Class'Class is allowed, and "is the same as" T'Class.
                  -- They are even conformant (checked with Gnat).
                  -- => Discard extra 'Class before they damage the rest of this algorithm
                  Good_Mark := Simple_Name (Strip_Attributes (Good_Mark));
               when others =>
                  -- Impossible
                  Impossible ("Attribute of Type_Profile = "
                              & Attribute_Kinds'Wide_Image (Attribute_Kind (Def)),
                              Declaration);
            end case;

         else
            Good_Mark := Simple_Name (Def);
            Attribute := None;
         end if;

         Decl := Corresponding_Name_Declaration (Good_Mark);
         if Is_From_Limited_View (Decl) then
            Decl := Get_Nonlimited_View (Decl);
         end if;

         case Declaration_Kind (Decl) is
            when An_Incomplete_Type_Declaration .. A_Tagged_Incomplete_Type_Declaration =>
               -- cannot take the Corresponding_First_Subtype of an incomplete type, go to the
               -- full type first
               --
               -- Decl can be Nil_Element if the full context is not available. TBH, it should always be
               -- available, and an enhancement request has been submitted to AdaCore about this.
               -- In the meantime, let's take back the incomplete type, and forget about the first subtype
               if Is_Nil (Decl) then
                  Decl := Corresponding_Name_Declaration (Good_Mark);
               else
                  Decl := Corresponding_First_Subtype (Corresponding_Full_Type_Declaration (Decl));
               end if;
            when A_Formal_Incomplete_Type_Declaration =>
               null;
            when others =>
               Decl := Corresponding_First_Subtype (Decl);
         end case;
         return (Access_Form  => Not_An_Access_Definition,
                 Attribute    => Attribute,
                 Name         => Names (Decl) (1),
                 Anon_Profile => null);
      end Build_Entry;

      function Build_Profile (Parameters : Parameter_Specification_List) return Profile_Table is
      -- Assert: parameters is not an empty list
      -- This function is written to avoid recursivity if there is no other multiple
      -- parameter declaration than the first one.

         Names_1    : constant Name_List     := Names (Parameters (Parameters'First));
         Entry_1    : constant Profile_Entry := Build_Entry (Object_Declaration_View
                                                             (Parameters (Parameters'First)));
         Result     : Profile_Table (List_Index range 1 .. Names_1'Length + Parameters'Length - 1);
         Result_Inx : Asis.List_Index;
      begin
         for I in List_Index range 1 .. Names_1'Length loop
            Result (I) := Entry_1;
         end loop;

         Result_Inx := Names_1'Length;
         for I in List_Index range Parameters'First + 1 .. Parameters'Last loop
            declare
               Names_Rest : constant Name_List := Names (Parameters (I));
            begin
               if Names_Rest'Length /= 1 then
                  return Result (1 .. Result_Inx) & Build_Profile (Parameters (I .. Parameters'Last));
               end if;

               Result_Inx := Result_Inx + 1;
               Result (Result_Inx) := Build_Entry (Object_Declaration_View (Parameters (I)));
            end;
         end loop;
         return Result;
      end Build_Profile;

      Result_Entry     : Profile_Entry;
      Good_Declaration : Asis.Declaration := Declaration;
      use Asis.Definitions;
   begin  --  Types_Profile
      if Declaration_Kind (Good_Declaration) in A_Generic_Instantiation then
         -- We must get the profile from the corresponding generic element
         Good_Declaration := Corresponding_Declaration (Good_Declaration);
      end if;

      case Declaration_Kind (Good_Declaration) is
         when A_Function_Declaration
            | An_Expression_Function_Declaration   -- Ada 2012
            | A_Function_Body_Declaration
            | A_Function_Renaming_Declaration
            | A_Function_Body_Stub
            | A_Generic_Function_Declaration
            | A_Formal_Function_Declaration
            =>
            Result_Entry := Build_Entry (Result_Profile (Good_Declaration));

         when An_Enumeration_Literal_Specification =>
            -- Profile for an enumeration litteral
            -- Like a parameterless function; go up two levels (type specification then type declaration)
            -- to find the return type.
            -- of the Enumaration_Literal_Specification
            -- Return immediately, since we know there are no parameters, and Parameter_Profile
            -- would choke on this.
            return (Formals_Length => 0,
                    Result_Type    => (Access_Form  => Not_An_Access_Definition,
                                       Attribute    => None,
                                       Name         => Names (Enclosing_Element
                                         (Enclosing_Element (Good_Declaration))) (1),
                                       Anon_Profile => null),
                    Formals        => (others => (Not_An_Access_Definition, None, Nil_Element, null)));
         when Not_A_Declaration =>
            if Definition_Kind (Good_Declaration) /= An_Access_Definition then
               Impossible ("Types_Profile: bad declaration", Good_Declaration);
            end if;

            case Access_Definition_Kind (Good_Declaration) is
               when An_Anonymous_Access_To_Procedure | An_Anonymous_Access_To_Protected_Procedure =>
                  Result_Entry := (Access_Form  => Not_An_Access_Definition,
                                   Attribute    => None,
                                   Name         => Nil_Element,
                                   Anon_Profile => null);
               when An_Anonymous_Access_To_Function  | An_Anonymous_Access_To_Protected_Function  =>
                  Result_Entry := Build_Entry (Access_To_Function_Result_Profile (Good_Declaration));
               when others =>
                  Impossible ("Types_Profile: bad access_definition", Good_Declaration);
            end case;
         when others => -- (generic) procedure or entry declaration
            Result_Entry := (Access_Form  => Not_An_Access_Definition,
                             Attribute    => None,
                             Name         => Nil_Element,
                             Anon_Profile => null);
      end case;

      declare
         function All_Parameter_Profile (D : Asis.Element) return Asis.Parameter_Specification_List is
         -- of course, an if-expression would do as well, but we still stick to Ada05
         begin
            if Element_Kind (D) = A_Declaration then
               return Parameter_Profile (D);
            else
               return Access_To_Subprogram_Parameter_Profile (D);
            end if;
         end All_Parameter_Profile;

         Parameters : constant Asis.Parameter_Specification_List := All_Parameter_Profile (Good_Declaration);
      begin
         if Parameters'Length = 0 then
            return (Formals_Length => 0,
                    Result_Type    => Result_Entry,
                    Formals        => (others => (Not_An_Access_Definition, None, Nil_Element, null)));
         else
            declare
               Profile : constant Profile_Table := Build_Profile (Parameters);
            begin
               return (Formals_Length => Profile'Length,
                       Result_Type    => Result_Entry,
                       Formals        => Profile);
            end;
         end if;
      end;
   end Types_Profile;

   -----------------
   -- All_Formals --
   -----------------

   function All_Formals (Profile : Asis.Parameter_Specification_List) return Defining_Name_List is
      -- Written to avoid recursvity, unless multiple names are declared
      Result : Asis.Defining_Name_List (Profile'Range);
   begin
      for P in Profile'Range loop
         declare
            N : constant Asis.Defining_Name_List := Names (Profile (P));
         begin
            if N'Length = 1 then
               Result (P) := N (1);
            elsif P = Profile'Last then
               return Result (1 .. P - 1) & N;
            else
               return Result (1 .. P - 1) & N & All_Formals (Profile (P + 1 .. Profile'Last));
            end if;
         end;
      end loop;
      return Result;
   end All_Formals;


   -----------------
   -- Formal_Name --
   -----------------

   function Formal_Name (Call : Asis.Element; Actual : Asis.List_Index) return Asis.Defining_Name is
      use Asis.Expressions;

      function Get_Formals_Profile return Asis.Parameter_Specification_List is
      begin
         if Declaration_Kind (Call) in A_Generic_Instantiation
           or Declaration_Kind (Call) = A_Formal_Package_Declaration
         then
            declare
               Formal_Part : Asis.Element_List := Generic_Formal_Part (Corresponding_Name_Declaration
                                                                       (Ultimate_Name
                                                                        (Generic_Unit_Name (Call))));
               Last : List_Index := Formal_Part'Last;
               Inx  : List_Index := Formal_Part'First;
            begin
               -- Remove use clauses
               while Inx <= Last loop
                  if Element_Kind (Formal_Part (Inx)) = A_Clause then
                     Formal_Part (Inx .. Last - 1) := Formal_Part (Inx + 1 .. Last);
                     Last := Last - 1;
                  else
                     Inx := Inx + 1;
                  end if;
               end loop;
               return Formal_Part (Formal_Part'First .. Last);
            end;
         else
            -- procedure, function, or entry call
            return Called_Profile (Call);
         end if;
      end Get_Formals_Profile;

      Actuals : constant Asis.Association_List             := Actual_Parameters (Call);
      Formals : constant Asis.Parameter_Specification_List := Get_Formals_Profile;
      I_A     : ASIS_Natural;
      I_F     : ASIS_Natural;
   begin  -- Formal_Name
      if Actual > Actuals'Length  -- Error
        or Is_Nil (Formals)       -- Dispatching call, or call to attribute
      then
         return Nil_Element;
      end if;

      if not Is_Nil (Formal_Parameter (Actuals (Actual))) then
         -- Easy case: Parameter given in named notation
         return Corresponding_Name_Definition (Formal_Parameter (Actuals (Actual)));
      end if;

      -- Parameter given in positional notation
      -- We can trust the order of parameters, and no parameter with default has been skipped!
      I_F := Formals'First;
      I_A := 0;
      loop
         declare
            These_Names : constant Asis.Element_List :=  Names (Formals (I_F));
         begin
            I_A := I_A + These_Names'Length;
            if I_A >= Actual then
               return These_Names (These_Names'Length - (I_A - Actual));
            end if;
         end;
         I_F := I_F + 1;
      end loop;
   end Formal_Name;

   -----------------
   -- Formal_Name --
   -----------------

   function Formal_Name (Assoc : Asis.Association) return Asis.Defining_Name is
      Call_Or_Instantiation : constant Asis.Element := Enclosing_Element (Assoc);
      Assoc_List            : constant Asis.Association_List := Actual_Parameters (Call_Or_Instantiation);
   begin
      for I in Assoc_List'Range loop
         if Is_Equal (Assoc_List (I), Assoc) then
            return Formal_Name (Call_Or_Instantiation, I);
         end if;
      end loop;

      -- Index must be found, by construction
      Impossible ("Formal_Name: Association not found in association list", Assoc);
   end Formal_Name;

   --------------------------
   -- Matching_Formal_Name --
   --------------------------

   function Matching_Formal_Name (Name : Asis.Defining_Name; Into : Asis.Declaration) return Asis.Defining_Name is
         This_Name : constant Wide_String := To_Upper (Defining_Name_Image (Name));
         Formals   : constant Asis.Parameter_Specification_List := Parameter_Profile (Into);
   begin
      for F in Formals'Range loop
         declare
            Other_Names : constant Asis.Defining_Name_List := Names (Formals (F));
         begin
            for N in Other_Names'Range loop
               if To_Upper (Defining_Name_Image (Other_Names (N))) = This_Name then
                  return Other_Names (N);
               end if;
            end loop;
         end;
      end loop;
      -- Not found here
      Impossible ("Matching_Formal_Name: not found", Name);
   end Matching_Formal_Name;

   -----------------------
   -- Actual_Expression --
   -----------------------

   function Actual_Expression (Call           : Asis.Element;
                               Formal         : Asis.Defining_Name;
                               Return_Default : Boolean := True) return Asis.Expression
   is
      use Asis.Expressions;
      Actuals : constant Asis.Association_List := Actual_Parameters (Call);
      Last_Is_Others    : Boolean := False;
      Good_Actuals_Last : Asis.List_Index;
   begin
      if Is_Dispatching_Call (Call) then
         return Nil_Element;
      end if;

      if Actuals /= Nil_Element_List then
         if Definition_Kind (Formal_Parameter (Actuals (Actuals'Last))) = An_Others_Choice then
            -- This happens only for the others => <> in a partial generic instantiation (in Ada 2012)
            -- but it doesn't harm to write it more general
            Last_Is_Others    := True;
            Good_Actuals_Last := Actuals'Last - 1;
         else
            Last_Is_Others    := False;
            Good_Actuals_Last := Actuals'Last;
         end if;

         for I in Asis.List_Index range Actuals'First .. Good_Actuals_Last loop
            if Is_Equal (Formal, Formal_Name (Call, I)) then
               return Actual_Parameter (Actuals (I));
            end if;
         end loop;

         -- Not found, can be covered by others
         if Last_Is_Others then
            return Actual_Parameter (Actuals (Actuals'Last));
         end if;
      end if;

      -- Otherwise, can still be covered by a default value
      if not Return_Default then
         return Nil_Element;
      end if;

      declare
         Formals : constant Asis.Association_List := Called_Profile (Call);
      begin
         for I in Formals'Range loop
            declare
               Formal_Names : constant Asis.Defining_Name_List := Names (Formals (I));
            begin
               for J in Formal_Names'Range loop
                  if Is_Equal (Formal_Names (J), Formal) then
                     return Initialization_Expression (Formals (I));
                  end if;
               end loop;
            end;
         end loop;
      end;

      -- Really not found
      return Nil_Element;
   end Actual_Expression;

   --------------------------------------------
   -- Corresponding_Static_Exception_Handler --
   --------------------------------------------

   function Corresponding_Static_Exception_Handler (Exc            : Asis.Defining_Name;
                                                    Where          : Asis.Element;
                                                    Include_Others : Boolean)
                                                    return Asis.Exception_Handler
   is
      use Asis.Expressions;
      Def_Name : Asis.Defining_Name;
      Context  : Asis.Element := Enclosing_Element (Where);
      Result   : Asis.Element := Nil_Element;

      function Matching_Handler (E : Asis.Element) return Asis.Exception_Handler is
         Handlers : constant Asis.Exception_Handler_List := Exception_Handlers (E);
      begin
         for H in Handlers'Range loop
            declare
               Choices : constant Asis.Element_List := Exception_Choices (Handlers (H));
            begin
               for C in Choices'Range loop
                  case Element_Kind (Choices (C)) is
                     when An_Expression =>
                        if Is_Equal (Def_Name, Corresponding_Name_Definition (Ultimate_Name (Choices (C)))) then
                           return Handlers (H);
                        end if;
                     when A_Definition =>
                        -- Handler for when others
                        if Include_Others then
                           return Handlers (H);
                        else
                           return Nil_Element;
                        end if;
                     when others =>
                        Impossible ("wrong result from Exception_Choices", Choices (C));
                  end case;
               end loop;
            end;
         end loop;
         -- Not found
         return Nil_Element;
      end Matching_Handler;

   begin  -- Corresponding_Static_Exception_Handler
      if Element_Kind (Exc) = A_Defining_Name then
         Def_Name := Exc;
      else
         Def_Name := Corresponding_Name_Definition (Simple_Name (Exc));
      end if;
      Def_Name := Ultimate_Name (Def_Name);

      while not Is_Nil (Context) loop
         case Element_Kind (Context) is
            when A_Declaration =>
               case Declaration_Kind (Context) is
                  when A_Procedure_Body_Declaration
                     | A_Function_Body_Declaration
                     | An_Entry_Body_Declaration
                     | A_Task_Body_Declaration
                       =>
                     Result := Matching_Handler (Context);
                     exit;  -- This is a callable construct (or task)
                  when A_Package_Body_Declaration =>
                     Result := Matching_Handler (Context);
                     exit when not Is_Nil (Result);
                     -- A package body is in a declarative part of a construct that does NOT
                     -- handle this exception (since it propagates directly outside)
                     -- => Skip one extra level, unless the body is a compilation unit
                     -- beware that this body may be separate, in which case it is replaced by
                     -- the corresponding stub
                     if Is_Subunit (Context) then
                        Context := Corresponding_Body_Stub (Context);
                     end if;
                     Context := Enclosing_Element (Context);
                     if not Is_Nil (Context) then -- Nil when the body is a compilation unit
                        Context := Enclosing_Element (Context);
                     end if;
                  when others =>
                     Context := Enclosing_Element (Context);
               end case;

            when A_Statement =>
               case Statement_Kind (Context) is
                  when An_Accept_Statement =>
                     Result := Matching_Handler (Context);
                     exit;  -- This is a callable construct
                  when A_Block_Statement =>
                     Result := Matching_Handler (Context);
                     exit when not Is_Nil (Result);
                     Context := Enclosing_Element (Context);
                  when others =>
                     Context := Enclosing_Element (Context);
               end case;

            when others =>
               Context := Enclosing_Element (Context);
         end case;
      end loop;

      return Result;
   end Corresponding_Static_Exception_Handler;


   -----------------------------------
   -- Is_Handled_Sequence_Container --
   -----------------------------------

   function Is_Handled_Sequence_Container (Element : in Asis.Element) return Boolean is
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Body_Declaration
                  | A_Procedure_Body_Declaration
                  | A_Package_Body_Declaration
                  | A_Task_Body_Declaration
                  | An_Entry_Body_Declaration
                  =>
                  return True;
               when others =>
                  return False;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Block_Statement
                  | An_Accept_Statement
                  | An_Extended_Return_Statement
                  =>
                  return True;
               when others =>
                  return False;
            end case;

         when others =>
            return False;
      end case;
   end Is_Handled_Sequence_Container;


   -----------------------
   -- Declarative_Items --
   -----------------------

   function Declarative_Items (Element : in Asis.Element; Include_Pragmas : in Boolean := False)
                              return Asis.Declaration_List
   is
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Body_Declaration
                 | A_Procedure_Body_Declaration
                 | A_Package_Body_Declaration
                 | A_Task_Body_Declaration
                 | An_Entry_Body_Declaration
                 =>
                  return Body_Declarative_Items (Element, Include_Pragmas);
               when A_Package_Declaration
                 | A_Generic_Package_Declaration
                 =>
                  return Visible_Part_Declarative_Items (Element, Include_Pragmas)
                       & Private_Part_Declarative_Items (Element, Include_Pragmas);
               when A_Protected_Body_Declaration =>
                  return Protected_Operation_Items (Element, Include_Pragmas);
               when others =>
                  Impossible ("Declarative_Items: invalid declaration kind", Element);
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
               when A_Block_Statement =>
                  return Block_Declarative_Items (Element, Include_Pragmas);
               when others =>
                  Impossible ("Declarative_Items: invalid statement kind", Element);
            end case;
         when others =>
            Impossible ("Declarative_Items: invalid element kind", Element);
      end case;
   end Declarative_Items;

   ----------------------------
   -- Enclosing_Program_Unit --
   ----------------------------

   function Enclosing_Program_Unit (Element          : Asis.Element;
                                    Including_Accept : Boolean      := False)
                                   return Asis.Defining_Name
   is
      My_Enclosing_Element : Asis.Element;
      Result               : Asis.Defining_Name;
   begin
      if Is_Nil (Element) then
         return Nil_Element;
      end if;

      My_Enclosing_Element := Enclosing_Element (Element);

      loop
         case Element_Kind (My_Enclosing_Element) is
            when Not_An_Element =>
               Result := Nil_Element;
               exit;

            when A_Declaration =>
               case Declaration_Kind (My_Enclosing_Element) is
                  when A_Procedure_Declaration
                     | A_Null_Procedure_Declaration   -- Ada 2005
                     | A_Procedure_Body_Declaration
                     --
                     | A_Function_Declaration
                     | An_Expression_Function_Declaration   -- Ada 2012
                     | A_Function_Body_Declaration
                     --
                     | A_Package_Declaration
                     | A_Package_Body_Declaration
                     --
                     | A_Task_Type_Declaration
                     | A_Single_Task_Declaration
                     | A_Task_Body_Declaration
                     --
                     | A_Protected_Type_Declaration
                     | A_Single_Protected_Declaration
                     | A_Protected_Body_Declaration
                     --
                     | An_Entry_Declaration
                     | An_Entry_Body_Declaration
                     --
                     | A_Procedure_Body_Stub
                     | A_Function_Body_Stub
                     | A_Package_Body_Stub
                     | A_Task_Body_Stub
                     | A_Protected_Body_Stub
                     --
                     | A_Generic_Procedure_Declaration
                     | A_Generic_Function_Declaration
                     | A_Generic_Package_Declaration
                     =>
                     Result := Names (My_Enclosing_Element) (1);
                     -- If Element was a defining name of a callable construct or task,
                     -- we are back to it. We must go one level higher.
                     if not Is_Equal (Result, Element) then
                        exit;
                     end if;

                  when others =>
                     null;

               end case;

            when A_Statement =>
               case Statement_Kind (My_Enclosing_Element) is
                  when An_Accept_Statement =>
                     if Including_Accept then
                       Result := Names (Corresponding_Entry
                                        (My_Enclosing_Element)
                                       ) (1);
                       exit;
                     end if;

                  when others =>
                     null;

               end case;

            when others=>
               null;

         end case;

         My_Enclosing_Element := Enclosing_Element(My_Enclosing_Element);

      end loop;

      return Result;

   exception
      when Asis.Exceptions.ASIS_Failed =>
         declare
            use Asis.Errors, Asis.Implementation;
         begin
            if Status = Not_Implemented_Error then
               -- ASIS bug [L702-009]
               -- Raised by Enclosing_Element in the case of a pragma which is not included in a unit
               return Nil_Element;
            end if;
         end;
         raise;
   end Enclosing_Program_Unit;


   --------------------------
   -- Expression_Type_Kind --
   --------------------------

   function Expression_Type_Kind (The_Element : Asis.Expression) return Asis.Type_Kinds is
      The_Type : constant Asis.Definition := Ultimate_Expression_Type (The_Element);
   begin
      if Is_Nil (The_Type) then
         return Not_A_Type_Definition;
      end if;

      return Type_Kind (The_Type);
   end Expression_Type_Kind;


   ---------------------------
   -- Expression_Usage_Kind --
   ---------------------------

   function Expression_Usage_Kind (Expr : Asis.Expression) return Expression_Usage_Kinds is
      use Asis.Clauses, Asis.Expressions;
      use Thick_Queries;
      Elem     : Asis.Element := Expr;
      Previous : Asis.Element;

      function Callable_Usage_Kind (Callable, Enclosed : Asis.Expression) return Expression_Usage_Kinds is
         Actuals : constant Asis.Association_List := Actual_Parameters (Callable);
         Formal  : Asis.Defining_Name;
      begin
         -- Find the position
         for I in Actuals'Range loop
            if Is_Equal (Actuals (I), Enclosed) then
               Formal := Formal_Name (Callable, I);
               if Is_Nil (Formal) then
                  -- Call to a dispatching operation
                  -- We don't know the mode => Unknown
                  return Unknown;
               end if;
               case Mode_Kind (Enclosing_Element (Formal)) is
                  when Not_A_Mode =>
                     Impossible ("Wrong mode in Usage_Kind", Formal_Name (Elem, I));
                  when A_Default_In_Mode | An_In_Mode =>
                     return Read;
                  when An_Out_Mode =>
                     return Write;
                  when An_In_Out_Mode =>
                     return Read_Write;
               end case;
            end if;
         end loop;
         -- If it is not a parameter, it must be part of the called name
         -- (implicit dereference of an access to procedure or entry)
         return Read;
      end Callable_Usage_Kind;

   begin  -- Expression_Usage_Kind
      -- Protected objects can only be read, first get rid of that special case:
      if Definition_Kind (Ultimate_Expression_Type (Expr)) = A_Protected_Definition then
         return Read;
      end if;

      -- Go up the expression until we find something that allows us to make a decision
      loop
         Previous := Elem;
         Elem     := Enclosing_Element (Elem);

         -- If Previous is a literal, a constant, or an in parameter, it can only be read
         -- unless it is the prefix of an attribute, where it is untouched
         case Expression_Kind (Previous) is
            when An_Integer_Literal
               | A_Real_Literal
               | A_String_Literal
               =>
               return Read;
            when An_Identifier =>
               case Declaration_Kind (Corresponding_Name_Declaration (Previous)) is
                  when A_Constant_Declaration =>
                     if Expression_Kind (Elem) = An_Attribute_Reference then
                        return Untouched;
                     else
                        return Read;
                     end if;
                  when A_Parameter_Specification =>
                     if Mode_Kind (Corresponding_Name_Declaration (Previous))
                        in A_Default_In_Mode .. An_In_Mode
                     then
                        if Expression_Kind (Elem) = An_Attribute_Reference then
                           return Untouched;
                        else
                           return Read;
                        end if;
                     end if;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;

         -- See enclosing context
         case Element_Kind (Elem) is
            when An_Expression =>
               case Expression_Kind (Elem) is
                  when A_Selected_Component =>
                     if Is_Equal (Previous, Prefix (Elem)) and then Is_Access_Expression (Previous) then
                        -- We have the prefix of an implicit dereference
                        -- => it is actually a Read of the variable
                        return Read;
                     end if;

                  when An_Identifier =>
                     Impossible ("enclosing element is an identifier", Elem);

                  when An_Indexed_Component
                    | A_Slice
                    =>
                     if not Is_Equal (Previous, Prefix (Elem)) then
                        -- Previous is part of the indexing or of the slice
                        return Read;
                     end if;

                     -- Previous is the prefix
                     if Is_Access_Expression (Previous) then
                        -- The prefix is an implicit dereference
                        -- => it is actually a Read of the variable
                        return Read;
                     end if;

                  when A_Type_Conversion
                    | A_Qualified_Expression
                     =>
                     null;  -- Go up

                  when An_Explicit_Dereference =>
                     -- Explicit dereference => the object is not modified
                     return Read;

                  when  A_Function_Call =>
                     -- Function call => must handle like procedure call, now (2012) that we have
                     -- [in] out parameters in functions!
                     -- We cannot access the profile of predefined operators, but fortunately all
                     -- operators must have parameters of mode in, so let's get rid of that first.
                     if Expression_Kind (Called_Simple_Name (Elem)) = An_Operator_Symbol then
                        return Read;
                     else
                        return Callable_Usage_Kind (Elem, Previous);
                     end if;
                  when An_Attribute_Reference =>
                     -- This is not an access to the object itself
                     return Untouched;

                  when others =>
                     -- Not a variable
                     return Read;
               end case;

            when A_Statement =>
               case Statement_Kind (Elem) is
                  when An_Assignment_Statement =>
                     if Is_Equal (Assignment_Variable_Name (Elem), Previous) then
                        return Write;
                     else
                        return Read;
                     end if;
                  when A_Procedure_Call_Statement
                     | An_Entry_Call_Statement
                     =>
                     return Callable_Usage_Kind (Elem, Previous);

                  when others =>
                     return Read;
               end case;

            when An_Association =>
               null;  -- Go up

            when A_Declaration =>
               case Declaration_Kind (Elem) is
                  when A_Renaming_Declaration =>
                     return Untouched;
                  when others =>
                     return Read;
               end case;

            when A_Clause =>
               if Clause_Kind (Elem) in A_Representation_Clause .. A_Component_Clause
                 and then Is_Equal (Previous, Representation_Clause_Name (Elem))
               then
                  -- The object is the target of the representation or component clause
                  return Untouched;
               else
                  return Read;
               end if;

            when others =>
               return Read;
         end case;
      end loop;

   end Expression_Usage_Kind;


   -------------------------
   -- Extended_Name_Image --
   -------------------------

   function Extended_Name_Image (Name_Elem               : Asis.Element;
                                 Silent_If_Inappropriate : Boolean := False) return Wide_String
   is
      use Asis.Expressions;
   begin
      case Element_Kind (Name_Elem) is
         when An_Expression =>
            case Expression_Kind (Name_Elem) is
               when A_Selected_Component =>
                  return Extended_Name_Image (Prefix (Name_Elem))
                    & '.'
                    & Name_Image (Selector (Name_Elem));
               when An_Identifier | An_Operator_Symbol | An_Enumeration_Literal =>
                  return Name_Image (Name_Elem);
               when An_Attribute_Reference =>
                  return Extended_Name_Image (Prefix (Name_Elem))
                    & '''
                    & Attribute_Name_Image (Name_Elem);
               when An_Explicit_Dereference =>
                  return Extended_Name_Image (Prefix (Name_Elem)) & ".all";
               when others =>
                  if Silent_If_Inappropriate then
                     return "";
                  end if;
                  Impossible ("Not a name in Extended_Name_Image", Name_Elem);
            end case;
         when A_Defining_Name =>
            return Defining_Name_Image (Name_Elem);
         when A_Pragma =>
            return Pragma_Name_Image (Name_Elem);
         when others =>
            if Silent_If_Inappropriate then
               return "";
            end if;
            Impossible ("Not a name in Extended_Name_Image", Name_Elem);
      end case;
   end Extended_Name_Image;


   --------------------------
   -- External_Call_Target --
   --------------------------

   function External_Call_Target (Call : Asis.Element) return Asis.Expression is
      use Asis.Expressions;
      Name          : Asis.Expression;
      Pref          : Asis.Expression;
      Enclosing_Obj : Asis.Element;
   begin
      if Expression_Kind (Call) = A_Function_Call then
         Name := Prefix (Call);
      else
         case Statement_Kind (Call) is
            when A_Procedure_Call_Statement | An_Entry_Call_Statement =>
               Name := Called_Name (Call);
            when A_Requeue_Statement | A_Requeue_Statement_With_Abort =>
               Name := Requeue_Entry_Name (Call);
            when others =>
               Impossible ("not a callable entity or requeue in External_Call_Target", Call);
         end case;
      end if;

      if Expression_Kind (Name) /= A_Selected_Component then
         -- Certainly not an external call
         return Nil_Element;
      end if;

      Pref := Prefix (Name);
      case Definition_Kind (Ultimate_Expression_Type (Pref)) is
         when A_Task_Definition =>
            return Pref;
         when A_Protected_Definition =>
            -- It is an external call unless the prefix designates a protected object
            -- that statically includes the location of the call.
            -- The precondition requires that the call be nested in a protected body
            Enclosing_Obj := Enclosing_Element (Call);
            while Declaration_Kind (Enclosing_Obj) /= A_Protected_Body_Declaration loop
               Enclosing_Obj := Enclosing_Element (Enclosing_Obj);
               if Is_Nil (Enclosing_Obj) then
                  -- Call not in a PTO => always external
                  return Pref;
               end if;
            end loop;

            declare
               Obj_Name : constant Wide_String := To_Upper (Full_Name_Image (Names (Enclosing_Obj) (1),
                                                                                    With_Profile => True));
               Pfx_Name : constant Wide_String := To_Upper (Full_Name_Image (Pref, With_Profile => True));
            begin
               if Pfx_Name'Length <= Obj_Name'Length
                 and then Obj_Name (1 .. Pfx_Name'Length) = Pfx_Name
               then
                  return Nil_Element;
               else
                  return Pref;
               end if;
            end;

         when others =>
            -- This includes the case where the prefix is the name of a protected
            -- *type*, since Ultimate_Expression_Type returns Nil_Element. But such
            -- calls are always internal calls.
            return Nil_Element;
      end case;

   end External_Call_Target;

   ---------------------
   -- Full_Name_Image --
   ---------------------

   function Full_Name_Image (The_Name     : in Asis.Element;
                             With_Profile : in Boolean := False) return Wide_String
   is
      use Asis.Expressions;

      Parent                   : Element;
      Anonymous_Count          : Natural;
      Extended_Return_Position : Natural;
      Decl_Name                : Asis.Defining_Name;

      function Anonymous_Subname return Wide_String is
      -- Returns a chain of Anonymous_Count times "_anonymous_.", except that the one at
      -- position Extended_Return_Position (from right to left) is replaced by "return."
         use Ada.Strings.Wide_Fixed;
      begin
         if Extended_Return_Position = 0 then
            return Anonymous_Count * "_anonymous_.";
         else
            return (Anonymous_Count - Extended_Return_Position) * "_anonymous_."
              & "return."
              & (Extended_Return_Position-1) * "_anonymous_.";
         end if;
      end Anonymous_Subname;

      function Simple_Name_Image (N : Asis.Defining_Name) return Wide_String is
         -- Adds profile to name if necessary
         Name_Image : constant Wide_String := Defining_Name_Image (N);
      begin
         if With_Profile
           -- A generic is not overloadable, therefore we don't add the profile
           and then not Is_Generic_Unit (Enclosing_Element (N))
         then
            return Name_Image & Profile_Image (N, With_Profile => True);
         else
            return Name_Image;
         end if;
      end Simple_Name_Image;

      Decl_Name_Enclosing : Asis.Element;
   begin   -- Full_Name_Image
      if Expression_Kind (The_Name) = An_Attribute_Reference then
         if Expression_Kind (Prefix (The_Name)) = A_Function_Call then
            return Full_Name_Image (Prefix (Prefix (The_Name)), With_Profile) & ''' & Attribute_Name_Image (The_Name);
         else
            return Full_Name_Image (Prefix (The_Name), With_Profile) & ''' & Attribute_Name_Image (The_Name);
         end if;
      end if;

      if Element_Kind (The_Name) = A_Defining_Name then
         Decl_Name := The_Name;
      else
         Decl_Name := Corresponding_Name_Definition (Simple_Name (The_Name));
      end if;

      -- Get rid of (annoying) special cases where we have no declaration:
      -- A predefined operator
      -- A name of a dispatching call
      if Is_Nil (Decl_Name) then
         -- Predefined operators are declared in Standard. If no profile is required, we can
         -- still build an image...
         if Expression_Kind (The_Name) /= An_Operator_Symbol or With_Profile then
            return "";
         end if;

         Parent := Enclosing_Element (The_Name);
         while Expression_Kind (Parent) = A_Selected_Component loop
            Parent := Enclosing_Element (Parent);
         end loop;
         if Is_Dispatching_Call (Parent) then
            -- Too bad! A dispatching operator...
            return "";
         end if;

         return "STANDARD." & Name_Image (The_Name);
      end if;

      -- Get rid of another annoying special case:
      -- A defining name that is actually part of a Defining_Expanded_Name
      -- (i.e. the composite name of a child unit).
      -- The full name is actually the Defining_Name of the enclosing construct
      Decl_Name_Enclosing := Enclosing_Element (Decl_Name);
      if Element_Kind (Decl_Name_Enclosing) = A_Defining_Name and then
        Defining_Name_Kind (Decl_Name_Enclosing) = A_Defining_Expanded_Name
      then
         return Simple_Name_Image (Decl_Name_Enclosing);
      end if;

      -- First, leave the declaration that encloses the name
      Parent := Enclosing_Element (Decl_Name_Enclosing);

      -- There are cases of nested definitions (enumeration litterals, implicit declarations inside
      -- derived types, instantiations...) => go up  until we find something that's the "real"
      -- parent (i.e. the previous element of the full name).
      --
      -- If we encounter unnamed loops or blocks, we count them, but continue to go up. This
      -- allows generating a junk name that includes as many "_anonymous_." as unnamed statements

      Anonymous_Count          := 0;
      Extended_Return_Position := 0;
      loop
         case Element_Kind (Parent) is
            when Not_An_Element =>
               -- No parent => compilation unit
               -- But can still be a proper body
               if Is_Subunit (Decl_Name_Enclosing) then
                  -- The full name is the same as the name of the stub
                  return Full_Name_Image (Names (Corresponding_Body_Stub(Decl_Name_Enclosing))(1),
                                          With_Profile);
               else
                  return Simple_Name_Image (Decl_Name);
               end if;

            when A_Declaration =>
               case Declaration_Kind (Parent) is
                  when A_Procedure_Declaration
                     | A_Null_Procedure_Declaration   --Ada 2005
                     | A_Function_Declaration
                     | An_Expression_Function_Declaration --Ada 2012
                     | A_Package_Declaration
                     | A_Task_Type_Declaration
                     | A_Single_Task_Declaration
                     | A_Protected_Type_Declaration
                     | A_Single_Protected_Declaration
                     | An_Entry_Declaration

                     | A_Procedure_Body_Declaration
                     | A_Function_Body_Declaration
                     | A_Package_Body_Declaration
                     | A_Task_Body_Declaration
                     | A_Protected_Body_Declaration
                     | An_Entry_Body_Declaration

                     | A_Procedure_Body_Stub
                     | A_Function_Body_Stub
                     | A_Package_Body_Stub
                     | A_Task_Body_Stub
                     | A_Protected_Body_Stub

                     | A_Generic_Procedure_Declaration
                     | A_Generic_Function_Declaration
                     | A_Generic_Package_Declaration

                     | A_Formal_Procedure_Declaration
                     | A_Formal_Function_Declaration
                       =>
                     return Full_Name_Image (Names (Parent) (1), With_Profile)
                       & '.'
                       & Anonymous_Subname
                       & Simple_Name_Image (Decl_Name);
                  when An_Ordinary_Type_Declaration =>
                     case Type_Kind (Type_Declaration_View (Parent)) is
                        when A_Record_Type_Definition
                           | A_Tagged_Record_Type_Definition
                           | A_Derived_Record_Extension_Definition
                             =>
                           -- No (anonymous) blocks can be nested in a type declaration
                           return Full_Name_Image (Names (Parent) (1), With_Profile)
                             & '.'
                             & Simple_Name_Image (Decl_Name);
                        when An_Access_Type_Definition =>
                           case Access_Type_Kind (Type_Declaration_View (Parent)) is
                              when Access_To_Subprogram_Definition =>
                                 -- No (anonymous) blocks can be nested in a type declaration
                                 return Full_Name_Image (Names (Parent) (1), With_Profile)
                                   & '.'
                                   & Simple_Name_Image (Decl_Name);
                              when others =>
                                 null;
                           end case;
                        when others =>
                           null;
                     end case;
                  when others =>
                     null;
               end case;

            when A_Statement =>
               case Statement_Kind (Parent) is
                  when  A_Loop_Statement .. A_Block_Statement =>
                     -- Statements that can have a name
                     if not Is_Nil (Statement_Identifier (Parent)) then
                        return Full_Name_Image (Statement_Identifier (Parent), With_Profile)
                          & '.'
                          & Anonymous_Subname
                          & Simple_Name_Image (Decl_Name);
                     end if;

                     Anonymous_Count := Anonymous_Count + 1;
                  when An_Accept_Statement =>
                     return Full_Name_Image (Names (Corresponding_Entry (Parent))(1), With_Profile)
                       & '.'
                       & Simple_Name_Image (Decl_Name);
                  when An_Extended_Return_Statement =>
                     -- This is now (2005) a new kind of anonymous scope. Count it in Anonymous_Count, but keep
                     -- its position in Extended_Return_Position to print "return." instead of "_anonymous_."
                     -- Fortunately, extended returns cannot be nested!
                     Anonymous_Count          := Anonymous_Count + 1;
                     Extended_Return_Position := Anonymous_Count;
                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;
         Parent := Enclosing_Element (Parent);
      end loop;
   end Full_Name_Image;


   -----------------------
   -- Includes_Renaming --
   -----------------------

   function Includes_Renaming (Path : Asis.Expression) return Boolean is
      use Asis.Expressions;
   begin
      case Expression_Kind (Path) is
         when An_Identifier =>
            return Declaration_Kind (Corresponding_Name_Declaration (Path)) in A_Renaming_Declaration;
         when A_Selected_Component =>
            return Includes_Renaming (Selector (Path)) or else Includes_Renaming (Prefix (Path));
         when A_Function_Call | An_Indexed_Component | A_Slice =>
            return Includes_Renaming (Prefix (Path));
         when An_Explicit_Dereference =>
            return Includes_Renaming (Prefix (Path));
         when A_Type_Conversion | A_Qualified_Expression =>
            return Includes_Renaming (Converted_Or_Qualified_Subtype_Mark (Path));
         when others =>
            Impossible ("Includes_Renaming called on " &
                        Expression_Kinds'Wide_Image (Expression_Kind (Path)),
                        Path);
      end case;
   end Includes_Renaming;


   ------------------------------
   -- Corresponding_Pragma_Set --
   ------------------------------

   function Corresponding_Pragma_Set (Element : in Asis.Element) return Pragma_Set is
      use Asis.Definitions, Asis.Expressions;

      Elem_Def_Name  : Asis.Defining_Name;
   begin
      if Is_Nil (Element) then
         return (others => False);
      end if;

      case Element_Kind (Element) is
         when A_Defining_Name =>
            Elem_Def_Name  := Element;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier =>
                  Elem_Def_Name  := Corresponding_Name_Definition (Element);
               when A_Selected_Component =>
                  Elem_Def_Name  := Corresponding_Name_Definition (Selector (Element));
               when others =>
                  -- Expression is neither An_Identifier, nor A_Selected_Component
                  -- Includes An_Attribute_Reference (T'Class)
                  return (others => False);
            end case;

            -- Element is neither A_Defining_Name, nor An_Identifier, nor A_Selected_Component
         when others =>
            return (others => False);
      end case;

      declare
         Element_Decl    : constant Asis.Declaration         := Enclosing_Element (Elem_Def_Name);
         Element_Pragmas : constant Asis.Pragma_Element_List := Corresponding_Pragmas (Element_Decl);
         Type_Name       : Asis.Expression;
         Result          : Pragma_Set := (others => False);
      begin
         for Pragma_Elt in Element_Pragmas'Range loop
            -- Retrieve the associations composing the current pragma
            declare
               Pragma_Associations : constant Asis.Association_List := Pragma_Argument_Associations (Element_Pragmas
                                                                                                     (Pragma_Elt));
            begin
               for Pragma_Assoc in Pragma_Associations'Range loop
                  -- Check if the pragma has been set on Element
                  begin
                     if Is_Equal (Elem_Def_Name,
                                  Corresponding_Name_Definition (Actual_Parameter
                                                                 (Pragma_Associations (Pragma_Assoc))))
                     then
                        Result (Pragma_Kind (Element_Pragmas (Pragma_Elt))) := True;
                     end if;
                  exception
                     when Asis.Exceptions.ASIS_Inappropriate_Element =>
                        -- Raised by Corresponding_Name_Definition of predefined "special" identifiers, like the "C"
                        -- in pragma convention (C, .. .).
                        -- Anyway, this is a junk element
                        null;
                  end;
               end loop;
            end;
         end loop;

         case Declaration_Kind (Element_Decl) is
            -- For variables and constants, add pragmas inherited from the type
            when A_Variable_Declaration                   -- Name : [aliased] Type          [:= Value];
              | A_Constant_Declaration                    -- Name : [aliased] constant Type  := Value;
              | A_Deferred_Constant_Declaration           -- Name : [aliased] constant Type; + see private part
              =>
               declare
                  Element_Type_Definition : constant Asis.Definition := Object_Declaration_View (Enclosing_Element
                                                                                                 (Element));
               begin
                  case Definition_Kind (Element_Type_Definition) is
                     when A_Component_Definition =>
                        Type_Name := Subtype_Simple_Name (Component_Definition_View (Element_Type_Definition));
                        if Expression_Kind (Type_Name) /= An_Attribute_Reference then
                           Result := Result or Corresponding_Pragma_Set (Names
                                                                         (Corresponding_First_Subtype
                                                                          (Corresponding_Name_Declaration
                                                                           (Type_Name)))(1));
                        end if;
                     when A_Subtype_Indication =>
                        Type_Name := Subtype_Simple_Name (Element_Type_Definition);
                        if Expression_Kind (Type_Name) /= An_Attribute_Reference then
                           Result := Result or Corresponding_Pragma_Set (Names
                                                                         (Corresponding_First_Subtype
                                                                          (Corresponding_Name_Declaration
                                                                           (Type_Name)))(1));
                        end if;

                     when others =>
                        null;
                  end case;
               end;

            -- For a subtype, check pragmas for the parent type
            when A_Subtype_Declaration =>
               Result := Result or Corresponding_Pragma_Set (Subtype_Simple_Name
                                                             (Type_Declaration_View
                                                              (Enclosing_Element (Elem_Def_Name))));

            -- For a derived type, check pragmas for the parent type
            when An_Ordinary_Type_Declaration =>
               declare
                  Element_Type_Definition : constant Asis.Definition := Type_Declaration_View (Enclosing_Element
                                                                                               (Elem_Def_Name));
               begin
                  case Type_Kind (Element_Type_Definition) is
                     when A_Derived_Type_Definition
                        | A_Derived_Record_Extension_Definition
                        =>
                        Result := Result or Corresponding_Pragma_Set (Subtype_Simple_Name
                                                                      (Parent_Subtype_Indication
                                                                       (Element_Type_Definition)));
                     when others =>
                        null;
                  end case;
               end;

            when A_Package_Declaration
               | A_Generic_Package_Declaration
               | A_Package_Body_Declaration
               =>
               -- For packages, categorizations are not returned by Corresponding_Pragmas
               -- Of course, this applies only to compilation units
               if Is_Compilation_Unit (Element_Decl) then
                  declare
                     Good_Decl : constant Asis.Declaration := Corresponding_Declaration (Element_Decl);
                     -- Categorization pragmas can be declared inside the specification, or after as compilation pragma
                     Other_Pragmas : constant Asis.Pragma_Element_List
                       := Pragmas (Good_Decl) & Compilation_Pragmas (Enclosing_Compilation_Unit (Good_Decl));
                     Kind : Asis.Pragma_Kinds;
                  begin
                     -- No need to check that categorization pragmas are for the current unit, the compiler
                     -- did it for us.
                     for P in Other_Pragmas'Range loop
                        Kind := Pragma_Kind (Other_Pragmas (P));
                        case Kind is
                           when A_Pure_Pragma
                              | A_Preelaborate_Pragma
                              | A_Preelaborable_Initialization_Pragma
                              | A_Shared_Passive_Pragma
                              | A_Remote_Types_Pragma
                              | A_Remote_Call_Interface_Pragma
                              =>
                              Result (Kind) := True;
                           when others =>
                              null;
                        end case;
                     end loop;
                  end;
               end if;
            when others =>
               null;
         end case;
         return Result;
      end;
   end Corresponding_Pragma_Set;


   ------------------------
   -- Is_Profile_Applied --
   ------------------------

   function Is_Profile_Applied (Element : in Asis.Element; Profile : Wide_String) return Boolean is
      use Asis.Expressions;
      Comp_Pragmas : constant Asis.Pragma_Element_List := Compilation_Pragmas (Enclosing_Compilation_Unit (Element));
   begin
      for P in Comp_Pragmas'Range loop
         if Pragma_Kind (Comp_Pragmas (P)) = A_Profile_Pragma then
            -- The name of the profile is always the first parameter of the pragma
            if To_Upper (Name_Image (Actual_Parameter (Pragma_Argument_Associations (Comp_Pragmas (P)) (1))))
              = Profile
            then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Is_Profile_Applied;


   -------------------------
   -- Range_Ultimate_Name --
   -------------------------

   function Range_Ultimate_Name (Range_Def : Asis.Definition) return Asis.Defining_Name is
      use Asis.Definitions, Asis.Expressions;

      Pfx  : Asis.Expression;
      Decl : Asis.Declaration;
      Def  : Asis.Definition;

      function Range_Position return ASIS_Positive is
         Range_Index : constant Asis.Expression_List := Attribute_Designator_Expressions
           (Range_Attribute (Range_Def));
      begin
         if Range_Index = Nil_Element_List then
            return 1;
         else
            return ASIS_Positive'Wide_Value (Static_Expression_Value_Image (Range_Index (1)));
         end if;
      end Range_Position;

   begin  -- Range_Ultimate_Name
      case Discrete_Range_Kind (Range_Def) is
         when Not_A_Discrete_Range =>
            Impossible ("Index_Subtype_Names: Not a discrete range", Range_Def);

         when A_Discrete_Subtype_Indication =>
            return Corresponding_Name_Definition (Subtype_Simple_Name (Range_Def));

         when A_Discrete_Range_Attribute_Reference =>
            Pfx := Simple_Name (Strip_Attributes (Prefix (Range_Attribute (Range_Def))));

            -- Here, Pfx is the good prefix simple name

            case Expression_Kind (Pfx) is
               when A_Function_Call =>  -- f(...)'Range
                  Decl := A4G_Bugs.Corresponding_Expression_Type (Pfx);
               when An_Explicit_Dereference => -- X.all'range
                  Decl := Access_Target_Type (Corresponding_Expression_Type_Definition (Prefix (Pfx)));
               when An_Indexed_Component =>   -- X (I)'Range
                  Decl := Corresponding_Expression_Type_Definition (Prefix (Pfx));
                  if Is_Access_Subtype (Decl) then
                     -- Indexing of an implicit dereference
                     Decl := Type_Declaration_View (Access_Target_Type (Decl));
                  end if;
                  Decl := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                          (Component_Definition_View
                                                           (Array_Component_Definition
                                                            (Decl))));
                  if Is_Access_Subtype (Decl) then  -- X(I) is an implicit dereference
                     Decl := Access_Target_Type (Decl);
                  end if;
               when others =>
                  Decl := Corresponding_Name_Declaration (Pfx);
            end case;

            loop
               case Declaration_Kind (Decl) is
                  when  An_Ordinary_Type_Declaration
                     | A_Subtype_Declaration
                     | A_Formal_Type_Declaration
                     =>
                     Def := Type_Declaration_View (Ultimate_Type_Declaration (Decl));
                     if Type_Kind (Def) in
                          An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition
                       or else Formal_Type_Kind (Def) in
                          A_Formal_Unconstrained_Array_Definition .. A_Formal_Constrained_Array_Definition
                     then
                        return Index_Subtypes_Names (Def) (Range_Position);
                     elsif Is_Access_Subtype (Decl) then
                        -- The 'Range was on an implicit dereference, take the accessed type
                        Decl := Access_Target_Type (Decl);
                     else
                        -- Not an array, must be T'Range where T is a discrete type, equivalent to T
                        return Corresponding_Name_Definition (Pfx);
                     end if;

                  when A_Variable_Declaration
                     | A_Constant_Declaration
                     | A_Deferred_Constant_Declaration
                     | A_Return_Variable_Specification
                     | A_Return_Constant_Specification
                     =>
                     Def := Object_Declaration_View (Decl);
                     if Definition_Kind (Def) = A_Type_Definition then -- anonymous array type
                        return Index_Subtypes_Names (Def) (Range_Position);
                     end if;
                     Decl := Corresponding_Name_Declaration (Subtype_Simple_Name (Def));

                  when A_Component_Declaration =>
                     Def := Component_Definition_View (Object_Declaration_View (Decl));
                     if Definition_Kind (Def) = A_Type_Definition then -- anonymous array type
                        return Index_Subtypes_Names (Def) (Range_Position);
                     end if;
                     Decl := Corresponding_Name_Declaration (Subtype_Simple_Name (Def));

                  when A_Parameter_Specification
                     | A_Formal_Object_Declaration
                     | An_Object_Renaming_Declaration
                     =>
                     Def := Object_Declaration_View (Decl);
                     if Element_Kind (Def) = An_Expression then
                        -- Should be a (possibly selected) name
                        Decl := Corresponding_Name_Declaration (Simple_Name (Def));
                     elsif Is_Access_Subtype (Def) then
                        Decl := Access_Target_Type (Def);
                     else
                        Decl := Corresponding_Name_Declaration (Subtype_Simple_Name (Def));
                     end if;

                  when A_Private_Type_Declaration | An_Incomplete_Type_Declaration =>
                     -- If this private (or incomplete) type was used as a range, it is necessarily from a place
                     -- where the full declaration is visible => we can take the full declaration without breaking
                     -- privacy. And of course, it cannot be a tagged incomplete type.
                     Decl := Corresponding_Full_Type_Declaration (Decl);

                  when others =>
                     Impossible ("Range_Ultimate_Name: unexpected declaration", Decl);
               end case;
            end loop;

         when A_Discrete_Simple_Expression_Range =>
            Decl := A4G_Bugs.Corresponding_Expression_Type (Lower_Bound (Range_Def));
            if Names (Decl) /= Nil_Element_List then
               return Names (Decl) (1);
            end if;

            -- Lower bound is of a universal type, try upper bound
            Decl := A4G_Bugs.Corresponding_Expression_Type (Upper_Bound (Range_Def));
            if Names (Decl) /= Nil_Element_List then
               return Names (Decl) (1);
            end if;

            -- Both bounds are of a universal type (Implicit Integer)
            return Nil_Element;
      end case;
   end Range_Ultimate_Name;


   --------------------
   -- Dimensionality --
   --------------------

   function Dimensionality (Type_Def : Asis.Type_Definition) return Asis.ASIS_Natural is
      use Asis.Definitions;
   begin
      if Type_Kind (Type_Def) = A_Constrained_Array_Definition
        or else Formal_Type_Kind (Type_Def) = A_Formal_Constrained_Array_Definition
      then
         return Discrete_Subtype_Definitions (Type_Def)'Length;

      elsif Type_Kind (Type_Def) = An_Unconstrained_Array_Definition
        or else Formal_Type_Kind (Type_Def) = A_Formal_Unconstrained_Array_Definition
      then
         return Index_Subtype_Definitions (Type_Def)'Length;

      else
         return 0;
      end if;
   end Dimensionality;

   --------------------------
   -- Index_Subtypes_Names --
   --------------------------

   function Index_Subtypes_Names (Type_Def : Asis.Type_Definition) return Asis.Element_List is
      use Asis.Definitions, Asis.Expressions;

   begin   -- Index_Subtypes_Names
      if Type_Kind (Type_Def) = A_Constrained_Array_Definition
        or else Formal_Type_Kind (Type_Def) = A_Formal_Constrained_Array_Definition
      then
         declare
            Index_List : Asis.Element_List := Discrete_Subtype_Definitions (Type_Def);
         begin
            for I in Index_List'Range loop
               Index_List (I) := Range_Ultimate_Name (Index_List (I));
            end loop;
            return Index_List;
         end;

      elsif Type_Kind (Type_Def) = An_Unconstrained_Array_Definition
        or else Formal_Type_Kind (Type_Def) = A_Formal_Unconstrained_Array_Definition
      then
         declare
            Index_List : Asis.Expression_List := Index_Subtype_Definitions (Type_Def);
         begin
            for I in Index_List'Range loop
               Index_List (I) := Corresponding_Name_Definition (Simple_Name (Index_List (I)));
            end loop;
            return Index_List;
         end;

      else
         Impossible ("Index_Subtypes_Names: not an array definition", Type_Def);
      end if;
   end Index_Subtypes_Names;

   --------------------------
   -- Is_Access_Expression --
   --------------------------

   function Is_Access_Expression (The_Element : Asis.Expression) return Boolean is
      Def : constant Asis.Definition := Corresponding_Expression_Type_Definition (The_Element);
   begin
      if Is_Nil (Def) then
         -- something that has no type (package name...)
         return False;
      end if;
      return Is_Access_Subtype (Def);
   end Is_Access_Expression;

   -------------------------------------
   -- Corresponding_Static_Predicates --
   -------------------------------------

   function Corresponding_Static_Predicates (Elem : in Asis.Element) return Asis.Element_List is
      use Asis.Definitions, Asis.Expressions;

      function Filter_Aspects (Aspects : Asis.Element_List; Name : Wide_String) return Asis.Element_List is
      -- Name is expected to be uppercase

         Result : Asis.Element_List (1 .. Aspects'Length);
         Count  : Asis_Natural := 0;
      begin
         for A in Aspects'Range loop
            if To_Upper (Extended_Name_Image (Aspect_Mark (Aspects (A)))) = Name then
               Count := Count + 1;
               Result (Count) := Aspects (A);
            end if;
         end loop;
         return Result (1 .. Count);
      end Filter_Aspects;

      Decl : Asis.Declaration;
   begin  -- Corresponding_Static_Predicates
      case Element_Kind (Elem) is
         when An_Expression =>
            case Expression_Kind (Elem) is
               when An_Identifier =>
                  -- if it is a type name, Corresponding_Expression_Type cannot be applied to it
                  Decl := Corresponding_Name_Declaration (Elem);
                  case Declaration_Kind (Decl) is
                     when A_Type_Declaration | A_Subtype_Declaration =>
                        null;
                     when others =>
                        Decl := A4G_Bugs.Corresponding_Expression_Type (Elem);
                  end case;
               when others =>
                  Decl := A4G_Bugs.Corresponding_Expression_Type (Elem);
            end case;
         when A_Declaration =>
            case Declaration_Kind (Elem) is
               when A_Type_Declaration | A_Subtype_Declaration =>
                  Decl := Elem;
               when others =>
                  return Nil_Element_List;
            end case;
         when others =>
            return Nil_Element_List;
      end case;

      if Is_Nil (Decl) then
         -- Anonymous type... Cannot have static predicate
         return Nil_Element_List;
      end if;

      -- Here we have a type or subtype declaration
      declare
         Decl_Aspects : constant Asis.Element_List := Filter_Aspects (Aspect_Specifications (Decl), "STATIC_PREDICATE");
         Def          : constant Asis.Definition := Type_Declaration_View (Decl);
      begin
         --## Rule off Avoid_Query ## for Corresponding_Parent_Subtype, we need a declaration, and we cannot have
         --                           anonymous stuff here
         case Declaration_Kind (Decl) is
            when An_Ordinary_Type_Declaration =>
               case Type_Kind (Def) is
                  when A_Derived_Type_Definition =>
                     return Decl_Aspects
                       & Corresponding_Static_Predicates (Corresponding_Parent_Subtype (Def));
                  when A_Derived_Record_Extension_Definition =>
                     -- we can have progenitors here
                     return Decl_Aspects
                       & Corresponding_Static_Predicates (Corresponding_Parent_Subtype (Def))
                       & Corresponding_Static_Predicates (Definition_Interface_List (Def));
                  when others =>
                     return Decl_Aspects;
               end case;
            when Declaration_Kinds'Succ (An_Ordinary_Type_Declaration) .. A_Type_Declaration'Last =>
               return Decl_Aspects;
            when A_Subtype_Declaration =>
               return Decl_Aspects
                    & Corresponding_Static_Predicates (Subtype_Simple_Name (Type_Declaration_View (Decl)));
            when A_Formal_Type_Declaration =>
               case Formal_Type_Kind (Def) is
                  when A_Formal_Derived_Type_Definition =>
                     -- we can have progenitors here
                     return Decl_Aspects
                       & Corresponding_Static_Predicates (Strip_Attributes (Subtype_Simple_Name (Def)))
                       & Corresponding_Static_Predicates (Definition_Interface_List (Def));
                  when others =>
                     return Decl_Aspects;
               end case;

            when others =>
               Impossible ("Corresponding_Static_Predicates: unexpected declaration", Decl);
         end case;
         --## Rule on Avoid_Query
      end;
   end Corresponding_Static_Predicates;

   -------------------------------------
   -- Corresponding_Static_Predicates --
   -------------------------------------

   function Corresponding_Static_Predicates (List : in Asis.Element_List) return Asis.Element_List is
   begin
      if Is_Nil (List) then
         return Nil_Element_List;
      end if;

      return Corresponding_Static_Predicates (List (List'First))
           & Corresponding_Static_Predicates (List (List'First + 1 .. List'Last));
   end Corresponding_Static_Predicates;

   -------------------
   -- Callable_Kind --
   -------------------

   Callable_Attributes : constant array (Asis.Attribute_Kinds) of Callable_Kinds
     := (An_Adjacent_Attribute          | A_Ceiling_Attribute    | A_Compose_Attribute         |
         A_Copy_Sign_Attribute          | An_Exponent_Attribute  | A_Floor_Attribute           |
         A_Fraction_Attribute           | An_Image_Attribute     | A_Leading_Part_Attribute    |
         A_Machine_Attribute            | A_Max_Attribute        | A_Min_Attribute             |
         A_Model_Attribute              | A_Pos_Attribute        | A_Pred_Attribute            |
         A_Remainder_Attribute          | A_Round_Attribute      | A_Rounding_Attribute        |
         A_Scaling_Attribute            | A_Succ_Attribute       | A_Truncation_Attribute      |
         An_Unbiased_Rounding_Attribute | A_Val_Attribute        | A_Value_Attribute           |
         A_Wide_Image_Attribute         | A_Wide_Value_Attribute | A_Wide_Wide_Image_Attribute |
         A_Wide_Wide_Value_Attribute
         => A_Function_Callable,

         An_Input_Attribute | An_Output_Attribute | A_Read_Attribute | A_Write_Attribute
         => A_Procedure_Callable,

         others
           => Not_A_Callable);

   function Callable_Kind (Element : Asis.Element) return Callable_Kinds is
      use Asis.Expressions;
      The_Declaration : Asis.Element;
   begin
      -- Go to the declaration
      case Element_Kind (Element) is
         when A_Declaration =>
            The_Declaration := Element;
         when A_Defining_Name | A_Definition =>
            The_Declaration := Enclosing_Element (Element);
         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier =>
                  The_Declaration := Corresponding_Name_Declaration (Element);
               when An_Enumeration_Literal =>
                  -- Always considered a function
                  return An_Enumeration_Callable;
               when A_Selected_Component =>
                  The_Declaration := Corresponding_Name_Declaration (Selector (Element));
               when An_Operator_Symbol =>
                  -- These are always functions
                  return A_Function_Callable;
               when An_Attribute_Reference =>
                  return Callable_Attributes (Attribute_Kind (Element));
               when others =>
                  Impossible ("Is_Callable_Construct called on "
                              & Expression_Kinds'Wide_Image (Expression_Kind (Element)),
                              Element);
            end  case;
         when A_Statement =>
            return Not_A_Callable;
         when others =>
            -- Impossible
            Impossible ("Callable_Kind called on "
                        & Element_Kinds'Wide_Image (Element_Kind (Element)),
                        Element);
      end case;

      case Declaration_Kind (The_Declaration) is
         when A_Procedure_Declaration
            | A_Null_Procedure_Declaration      --Ada 2005
            | A_Procedure_Instantiation
            | A_Procedure_Body_Declaration
            | A_Procedure_Renaming_Declaration
            | A_Procedure_Body_Stub
            | A_Formal_Procedure_Declaration
              =>
            return A_Procedure_Callable;

         when A_Function_Declaration
            | A_Function_Instantiation
            | An_Expression_Function_Declaration --Ada 2012
            | A_Function_Body_Declaration
            | A_Function_Renaming_Declaration
            | A_Function_Body_Stub
            | A_Formal_Function_Declaration
              =>
            return A_Function_Callable;

         when An_Enumeration_Literal_Specification =>
            return An_Enumeration_Callable;

         when An_Entry_Declaration
            | An_Entry_Body_Declaration
              =>
            if Is_Task_Entry (The_Declaration) then
               return A_Task_Entry_Callable;
            else
               return A_Protected_Entry_Callable;
            end if;

         when others =>
            return Not_A_Callable;
      end case;
   end Callable_Kind;


   ---------------------------
   -- Is_Callable_Construct --
   ---------------------------

   function Is_Callable_Construct (Element : Asis.Element) return Boolean is
   begin
      return Callable_Kind (Element) /= Not_A_Callable;
   end Is_Callable_Construct;


   ----------------------
   -- Is_Static_Object --
   ----------------------

   function Is_Static_Object (Obj : Asis.Expression) return Boolean is
      use Asis.Expressions;
   begin
      case Expression_Kind (Obj) is
         when An_Identifier =>
            return not Is_Nil (Ultimate_Name (Obj));
         when An_Explicit_Dereference =>
            return False;
         when An_Indexed_Component =>
            if Is_Access_Expression (Prefix (Obj)) then
               -- Implicit dereference
               return False;
            end if;

            if not Is_Static_Object (Prefix (Obj)) then
               return False;
            end if;

            declare
               Indexes : constant Asis.Expression_List := Index_Expressions (Obj);
            begin
               for I in Indexes'Range loop
                  if  not Is_Static_Expression (Indexes (I)) then
                     return False;
                  end if;
               end loop;
               return True;
            end;
         when A_Slice =>
            if Is_Access_Expression (Prefix (Obj)) then
               -- Implicit dereference
               return False;
            end if;

            return Is_Static_Object (Prefix (Obj))
              and then Discrete_Constraining_Lengths (Slice_Range (Obj), Follow_Access => False) (1) /= Not_Static;
         when A_Selected_Component =>
            case Declaration_Kind (Corresponding_Name_Declaration (Simple_Name (Prefix (Obj)))) is
               when A_Constant_Declaration
                  | A_Variable_Declaration
                  | A_Parameter_Specification
                  | A_Formal_Object_Declaration
                    =>
                  -- Prefix is an object
                  if Is_Access_Expression (Prefix (Obj)) then
                     -- Implicit dereference
                     return False;
                  end if;
                  return Is_Static_Object (Selector (Obj)) and then Is_Static_Object (Prefix (Obj));
               when others =>
                  -- Prefix must be a unit name
                  return Is_Static_Object (Selector (Obj));
            end case;
         when A_Type_Conversion | A_Qualified_Expression =>
            return Is_Static_Object (Converted_Or_Qualified_Expression (Obj));
         when others =>
            -- Not an object
            return False;
      end case;
   end Is_Static_Object;

   ----------------------------
   -- Is_Predefined_Operator --
   ----------------------------

   function Is_Predefined_Operator (Decl : Asis.Declaration) return Boolean is
   -- Expected declaration kind:
   --    A_Function_Declaration
   --    A_Function_Body_Declaration
   -- (of operator)
   -- Returns True if the operator is identical to a predefined one.
      use Asis.Definitions, Asis.Expressions;

      Profile : constant Profile_Descriptor := Types_Profile (Decl);
      Temp    : Asis.Element;
      Name    : constant Asis.Defining_Name := Names (Decl) (1);
      Kind    : constant Operator_Kinds := Operator_Kind (Name);

      Operation_Ultimate_Type : constant Asis.Definition
        := Type_Declaration_View (Ultimate_Type_Declaration (Enclosing_Element (Profile.Formals (1).Name)));

      function Array_Dimensions (Arr_Def : Asis.Definition) return Asis.List_Index is
      -- How many dimensions in provided array declaration ?
      begin
         if Type_Kind (Arr_Def) = A_Constrained_Array_Definition then
            return Discrete_Subtype_Definitions (Arr_Def)'Length;
         else
            -- unconstrained array
            return Index_Subtype_Definitions (Arr_Def)'Length;
         end if;
      end Array_Dimensions;

      function Is_Type (N : Asis.Defining_Name; Value : Wide_String; Or_Derived : Boolean := False) return Boolean is
      -- True if the ultimate type of N is Value
         D : Asis.Declaration := Enclosing_Element (N);
      begin
         if Or_Derived then
            D := Ultimate_Type_Declaration (D);
         end if;
         return To_Upper (Full_Name_Image (Names (D) (1))) = Value;
      end Is_Type;

   begin   -- Is_Predefined_Operator
      if Profile.Formals_Length = 1 then
         -- Unary operators

         -- Eliminate weird cases (not homogenous, access, class...) that cannot be predefined
         -- We purposedly ignore the 'Base attribute
         if not Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
           or Profile.Formals (1).Access_Form /= Not_An_Access_Definition
           or Profile.Formals (1).Attribute = Class
           or Profile.Result_Type.Attribute = Class
         then
            return False;
         end if;

         case Type_Kind (Operation_Ultimate_Type) is
            when A_Signed_Integer_Type_Definition
               | A_Floating_Point_Definition
               | An_Ordinary_Fixed_Point_Definition
               | A_Decimal_Fixed_Point_Definition
               =>
               -- All unary operators except "not" are predefined
               return Kind /= A_Not_Operator;
            when A_Modular_Type_Definition =>
               -- All unary operators are predefined
               return True;
            when An_Enumeration_Type_Definition =>
               -- Only Boolean has predefined operators
               return Is_Type (Profile.Formals (1).Name, "STANDARD.BOOLEAN", Or_Derived => True)
                 and then Kind = A_Not_Operator;
            when A_Constrained_Array_Definition
               | An_Unconstrained_Array_Definition
               =>
               if Array_Dimensions (Operation_Ultimate_Type) /= 1 then
                  return False;
               end if;
               -- Temp <- True component type name definition
               -- Get rid of 'Base! (cannot be 'Class)
               Temp := Corresponding_Name_Definition (Strip_Attributes
                                                      (Subtype_Simple_Name
                                                       (Component_Definition_View
                                                        (Array_Component_Definition
                                                        (Operation_Ultimate_Type)))));
               -- Boolean array?
               if Is_Type (Temp, "STANDARD.BOOLEAN", Or_Derived => True) then
                  return Kind = A_Not_Operator;
               end if;
               return False;
            when others =>
               return False;
         end case;
      end if;

      -- Binary operators

      -- Special case: "**" on floating point types
      if Kind = An_Exponentiate_Operator then
         return Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
           and then Profile.Formals (1).Access_Form = Not_An_Access_Definition
           and then Profile.Formals (2).Access_Form = Not_An_Access_Definition
           and then Type_Kind (Operation_Ultimate_Type) = A_Floating_Point_Definition
           and then Is_Type (Profile.Formals (2).Name, "STANDARD.INTEGER");
      end if;

      -- Special case: "*" and "/" on fixed point types
      if Kind in A_Multiply_Operator .. A_Divide_Operator then
         return Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
           and then Profile.Formals (1).Access_Form = Not_An_Access_Definition
           and then Profile.Formals (2).Access_Form = Not_An_Access_Definition
           and then Type_Kind (Operation_Ultimate_Type) in Fixed_Type_Kinds
           and then Is_Type (Profile.Formals (2).Name, "STANDARD.INTEGER");
      end if;

      -- Eliminate weird cases (not homogenous, access, class...) that cannot be predefined
      -- We purposedly ignore the 'Base attribute
      if Kind in Relational_Operators then
         if not Is_Equal (Profile.Formals (1).Name, Profile.Formals (2).Name)
           or not Is_Type (Profile.Result_Type.Name, "STANDARD.BOOLEAN", Or_Derived => True)
           or Profile.Formals (1).Access_Form /= Not_An_Access_Definition
           or Profile.Formals (2).Access_Form /= Not_An_Access_Definition
           or Profile.Formals (1).Attribute = Class
           or Profile.Formals (2).Attribute = Class
           or Profile.Result_Type.Attribute = Class
         then
            return False;
         end if;
      else
         if not Is_Equal (Profile.Formals (1).Name, Profile.Formals (2).Name)
           or not Is_Equal (Profile.Formals (1).Name, Profile.Result_Type.Name)
           or Profile.Formals (1).Access_Form /= Not_An_Access_Definition
           or Profile.Formals (2).Access_Form /= Not_An_Access_Definition
           or Profile.Formals (1).Attribute = Class
           or Profile.Formals (2).Attribute = Class
           or Profile.Result_Type.Attribute = Class
         then
            return False;
         end if;
      end if;

      -- Special case: "=" and "/=" of limited types
      if Kind in Equality_Operators and then Is_Limited (Profile.Formals (1).Name) then
         return False;
      end if;

      case Type_Kind (Operation_Ultimate_Type) is
         when A_Signed_Integer_Type_Definition
            | A_Floating_Point_Definition
            | An_Ordinary_Fixed_Point_Definition
            | A_Decimal_Fixed_Point_Definition
            =>
            return Kind in Adding_Operators
              or Kind in Multiplying_Operators
              or Kind in Relational_Operators;
         when A_Modular_Type_Definition =>
            return Kind in Adding_Operators
              or Kind in Multiplying_Operators
              or Kind in Logical_Operators
              or Kind in Relational_Operators;
         when A_Constrained_Array_Definition
            | An_Unconstrained_Array_Definition
            =>
            if Array_Dimensions (Operation_Ultimate_Type) /= 1 then
               return Kind in Equality_Operators;
            end if;

            -- Temp <- True component type name definition
            -- Remove 'Base (cannot be 'Class)
            Temp := Corresponding_Name_Definition (Strip_Attributes
                                                   (Subtype_Simple_Name
                                                    (Component_Definition_View
                                                     (Array_Component_Definition
                                                      (Operation_Ultimate_Type)))));
            -- Boolean array?
            if Is_Type (Temp, "STANDARD.BOOLEAN", Or_Derived => True) then
               return Kind in Logical_Operators
                 or Kind in Relational_Operators
                 or Kind = A_Concatenate_Operator;
            end if;

            -- Discrete array ?
            if Type_Kind (Enclosing_Element (Temp)) in Discrete_Type_Kinds then
               return Kind in Relational_Operators
                 or Kind = A_Concatenate_Operator;
            end if;

            return Kind in Equality_Operators
              or Kind = A_Concatenate_Operator;
         when others =>
            return Kind in Equality_Operators;
      end case;
   end Is_Predefined_Operator;

   -------------------
   -- Is_Task_Entry --
   -------------------

   function Is_Task_Entry (Declaration : Asis.Declaration) return Boolean is
      use Asis.Definitions, Asis.Expressions;
      Enclosing : Asis.Declaration := Enclosing_Element (Declaration);
   begin
      case Definition_Kind (Enclosing) is
         when  A_Task_Definition =>
            return True;
         when A_Type_Definition =>
            -- Must be A_Derived_Type_Defintion
            Enclosing := Ultimate_Type_Declaration (Corresponding_Name_Declaration
                                                    (Subtype_Simple_Name
                                                     (Parent_Subtype_Indication (Enclosing))));
            return Declaration_Kind (Enclosing) = A_Task_Type_Declaration;
         when others =>
            return False;
      end case;
   end Is_Task_Entry;


   -------------------------
   -- Subtype_Simple_Name --
   -------------------------

   function Subtype_Simple_Name (Definition : Asis.Definition) return Asis.Expression is
   begin
      return Simple_Name (Asis.Definitions.Subtype_Mark (Definition));   --##RULE LINE OFF Avoid_Query
   end Subtype_Simple_Name;

   ------------------------
   -- First_Subtype_Name --
   ------------------------

   function First_Subtype_Name (The_Subtype : Asis.Element) return Asis.Expression is
      use Asis.Expressions;
      ST   : Asis.Declaration;
      Mark : Asis.Expression;
   begin
      case Element_Kind (The_Subtype) is
         when A_Defining_Name =>
            ST := Enclosing_Element (The_Subtype);
         when An_Expression =>
            if Attribute_Kind (The_Subtype) = A_Base_Attribute then
               ST := Corresponding_Name_Declaration (Simple_Name (Prefix (The_Subtype)));
            else
               ST := Corresponding_Name_Declaration (Simple_Name (The_Subtype));
            end if;
         when others =>
            Impossible ("Wrong element kind in First_Subtype_Name", The_Subtype);
      end case;
      if Declaration_Kind (ST) /= A_Subtype_Declaration then
         -- The_Subtype was already the first named subtype
         if Attribute_Kind (The_Subtype) = A_Base_Attribute then
            return Prefix (The_Subtype);
         else
            return The_Subtype;
         end if;
      end if;

      -- We must unwind subtypes up to the last subtype (but not up to the type as
      -- Corresponding_First_Subtype would do). We ignore the 'Base attribute,
      -- but return the 'Class attribute
      -- We do not use Corresponding_Last_Subtype, because the ASIS standard does not
      -- specify what happens in the case of a subtype of a 'Class.
      loop
         Mark := Subtype_Simple_Name (Type_Declaration_View (ST));
         case Expression_Kind (Mark) is
            when An_Identifier =>
               ST := Corresponding_Name_Declaration (Mark);
               if Declaration_Kind (ST) /= A_Subtype_Declaration then
                  return Mark;
               end if;
            when An_Attribute_Reference =>
               case Attribute_Kind (Mark) is
                  when A_Base_Attribute =>
                     ST := Corresponding_Name_Declaration (Simple_Name (Prefix (Mark)));
                     if Declaration_Kind (ST) /= A_Subtype_Declaration then
                        return Prefix (Mark);
                     end if;
                  when A_Class_Attribute =>
                     return Mark;
                  when others =>
                     Impossible ("First_Subtype_Name: unexpected attribute", Mark);
               end case;
            when others =>
               Impossible ("First_Subtype_Name: unexpected mark", Mark);
         end case;
      end loop;
   end First_Subtype_Name;

   -----------------------
   -- Is_Access_Subtype --
   -----------------------

   function Is_Access_Subtype (The_Subtype : Asis.Element) return Boolean is
      Decl     : Asis.Declaration;
      Good_Def : Asis.Definition;
   begin
      case Element_Kind (The_Subtype) is
         when A_Defining_Name =>
              Decl := Enclosing_Element (The_Subtype);
         when A_Definition =>
            -- This may be a definition of an anonymous access or array type for which there is
            -- no declaration
            case Definition_Kind (The_Subtype) is
               when An_Access_Definition => -- ASIS 2005
               -- No declaration here, but cannot be a derived type
                  return True;
               when A_Task_Definition
                  | A_Protected_Definition
                  =>
                  return False;
               when A_Type_Definition =>
                  if Type_Kind (The_Subtype) in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition then
                     return False;
                  end if;
               when A_Formal_Type_Definition =>
                  if Formal_Type_Kind (The_Subtype)
                     in A_Formal_Unconstrained_Array_Definition .. A_Formal_Constrained_Array_Definition
                  then
                     return False;
                  end if;
               when others =>
                  null;
            end case;
            Decl := Enclosing_Element (The_Subtype);
            while Element_Kind (Decl) /= A_Declaration loop -- There might be several levels of nested definitions
               Decl := Enclosing_Element (Decl);
            end loop;
         when A_Declaration =>
            Decl := The_Subtype;
         when others =>
            Impossible ("Is_Access_Subtype: inappropriate element kind", The_Subtype);
      end case;

      Good_Def  := Type_Declaration_View (Ultimate_Type_Declaration (Decl));
      return    Type_Kind        (Good_Def) = An_Access_Type_Definition
        or else Formal_Type_Kind (Good_Def) = A_Formal_Access_Type_Definition;
   end Is_Access_Subtype;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (Outer : Asis.Compilation_Unit; Inner : Asis.Compilation_Unit) return Boolean is
      use Asis.Compilation_Units;

      Good_Inner : Asis.Compilation_Unit := Inner;
   begin
      -- Get rid of subunits
      while Unit_Kind (Good_Inner) in A_Subunit loop
         Good_Inner := Corresponding_Subunit_Parent_Body (Good_Inner);
      end loop;

      while not Is_Nil (Good_Inner) loop
         if Is_Equal (Outer, Good_Inner) then
            return True;
         end if;
         Good_Inner := Corresponding_Parent_Declaration (Good_Inner);
      end loop;

      return False;
   end Is_Ancestor;

   ----------------------
   -- Is_Array_Subtype --
   ----------------------

   function Is_Array_Subtype (The_Subtype : Asis.Element) return Boolean is
      use Asis.Expressions;

      Good_Name : Asis.Expression;
      Good_Def  : Asis.Definition;
   begin
      case Element_Kind (The_Subtype) is
         when A_Declaration =>
            Good_Def := Type_Declaration_View (Ultimate_Type_Declaration (The_Subtype));
         when A_Definition =>
            Good_Def := The_Subtype;
            if Definition_Kind (Good_Def) = A_Subtype_Indication then
               Good_Name := Subtype_Simple_Name (Good_Def);
               if Expression_Kind (Good_Name) = An_Attribute_Reference then
                  -- 'Base and 'Class cannot be arrays
                  return False;
               end if;
               Good_Def := Type_Declaration_View (Ultimate_Type_Declaration
                                                  (Corresponding_Name_Declaration (Good_Name)));
            end if;
         when A_Defining_Name =>
            Good_Def := Type_Declaration_View (Ultimate_Type_Declaration (Enclosing_Element (The_Subtype)));
         when An_Expression =>
            Good_Name := Simple_Name (The_Subtype);
            if Expression_Kind (Good_Name) = An_Attribute_Reference then
               -- 'Base and 'Class cannot be arrays
               return False;
            end if;
            Good_Def := Type_Declaration_View (Ultimate_Type_Declaration (Corresponding_Name_Declaration (Good_Name)));
         when others =>
            return False;
      end case;

      if Type_Kind (Good_Def)
         in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition
      then
         return True;
      elsif Formal_Type_Kind (Good_Def)
            in A_Formal_Unconstrained_Array_Definition .. A_Formal_Constrained_Array_Definition
      then
         return True;
      else
         return False;
      end if;
   end Is_Array_Subtype;


   --------------------------
   -- Is_Character_Subtype --
   --------------------------

   function Is_Character_Subtype (The_Subtype : Asis.Element) return Boolean is
      use Asis.Definitions, Asis.Expressions;

      Good_Name : Asis.Expression;
      Good_Decl : Asis.Declaration;
      Good_Def  : Asis.Definition;
   begin
      case Element_Kind (The_Subtype) is
         when A_Declaration =>
            Good_Decl := The_Subtype;
         when A_Definition =>
            Good_Decl := Enclosing_Element (The_Subtype);
         when A_Defining_Name =>
            Good_Decl := Enclosing_Element (The_Subtype);
         when An_Expression =>
            Good_Name := Simple_Name (The_Subtype);
            if Attribute_Kind (Good_Name) = A_Base_Attribute then
               Good_Name := Strip_Attributes (Good_Name);
            end if;
            Good_Decl := Corresponding_Name_Declaration (Good_Name);
         when others =>
            return False;
      end case;

      Good_Def := Type_Declaration_View (Ultimate_Type_Declaration (Good_Decl));
      if Type_Kind (Good_Def) /= An_Enumeration_Type_Definition then
         return False;
      end if;

      -- Check these special cases that may raise Storage_Error from Enumeration_Literal_Declarations:
      declare
         Full_Name : constant Wide_String := To_Upper (Full_Name_Image (Names (Enclosing_Element (Good_Def)) (1)));
      begin
         if Full_Name = "STANDARD.WIDE_CHARACTER" or else Full_Name = "STANDARD.WIDE_WIDE_CHARACTER" then
            return True;
         end if;
      end;

      declare
         Lits : constant Asis.Declaration_List := Enumeration_Literal_Declarations (Good_Def);
      begin
         for I in Lits'Range loop
            if Defining_Name_Kind (Names (Lits (I)) (1)) = A_Defining_Character_Literal then
               return True;
            end if;
         end loop;
         return False;
      end;
   end Is_Character_Subtype;

   ---------------------------
   -- Is_Class_Wide_Subtype --
   ---------------------------

   function Is_Class_Wide_Subtype (The_Subtype : Asis.Element) return Boolean is
      use Asis.Expressions;

      ST        : Asis.Declaration := The_Subtype;
      Good_Name : Asis.Expression;
   begin
      loop
         case Element_Kind (ST) is
            when A_Declaration =>
               if Declaration_Kind (ST) /= A_Subtype_Declaration then
                  return False;
               end if;
               ST := Type_Declaration_View (ST);
            when A_Definition =>
               -- Get rid of easy cases
               case Definition_Kind (ST) is
                  when An_Access_Definition =>
                     -- 2005: an anonymous access type => not class wide
                     return False;
                  when A_Type_Definition =>
                     -- Plain type => not class wide
                     return False;
                  when A_Subtype_Indication =>
                     ST := Subtype_Simple_Name (ST);
                  when others =>
                     return False;
               end case;
            when A_Defining_Name =>
               ST := Enclosing_Element (ST);
            when An_Expression =>
               Good_Name := Simple_Name (ST);
               if Expression_Kind (Good_Name) = An_Attribute_Reference then
                  return Attribute_Kind (Good_Name) = A_Class_Attribute;
               end if;
               ST := Corresponding_Name_Declaration (Good_Name);
            when others =>
               return False;
         end case;
      end loop;
   end Is_Class_Wide_Subtype;

   -------------------------
   -- Is_Compilation_Unit --
   -------------------------

   function Is_Compilation_Unit (Element : Asis.Element) return Boolean is
   begin
      return Is_Equal (Element, Unit_Declaration (Enclosing_Compilation_Unit (Element)));
   end Is_Compilation_Unit;

   -------------------
   -- Is_Controlled --
   -------------------

   function Is_Controlled (The_Element : Asis.Element) return Boolean is
      use Asis.Expressions;
      The_Subtype : Asis.Declaration := The_Element;
      -- TBH: will be the subtype's declaration after resolving other forms
   begin
      loop
         case Element_Kind (The_Subtype) is
            when A_Declaration =>
               case Declaration_Kind (The_Subtype) is
                  when A_Type_Declaration =>
                     exit;
                  when A_Subtype_Declaration =>
                     The_Subtype := Corresponding_First_Subtype (The_Subtype);
                  when A_Variable_Declaration
                     | A_Constant_Declaration
                     | A_Deferred_Constant_Declaration
                     | A_Component_Declaration
                     | A_Parameter_Specification
                     | A_Return_Variable_Specification
                     | A_Return_Constant_Specification
                     | A_Formal_Object_Declaration
                     | An_Object_Renaming_Declaration
                     =>
                     The_Subtype := Object_Declaration_View (The_Subtype);
                     if Definition_Kind (The_Subtype) /= A_Subtype_Indication then
                        -- Anonymous stuff cannot be controlled
                        return False;
                     end if;
                     The_Subtype := Subtype_Simple_Name (The_Subtype);
                  when others =>
                     return False;
               end case;
            when A_Definition | A_Defining_Name =>
               The_Subtype := Enclosing_Element (The_Subtype);
            when An_Expression =>
               if Expression_Kind (The_Subtype) = An_Identifier
                 and then Declaration_Kind (Corresponding_Name_Declaration (The_Subtype))
                          in An_Ordinary_Type_Declaration .. A_Subtype_Declaration
               then
                  -- This identifier is a (sub)type name
                  The_Subtype := Corresponding_Name_Declaration (The_Subtype);
               else
                  -- Real expression
                  The_Subtype := Corresponding_Expression_Type_Definition (The_Subtype);
               end if;
            when Not_An_Element =>
               -- we hit some strange beast, like fixed point multiplication, etc...
               -- anyway, can't be controlled
               return False;
            when others =>
               Impossible ("Is_Controlled: bad element", The_Element);
         end case;
      end loop;

      -- Here we have a type declaration, but beware of root and universal types (certainly not controlled)
      if Type_Kind (Type_Declaration_View (The_Subtype)) = A_Root_Type_Definition then
         return False;
      end if;
      declare
         Ultimate_Ancestor : constant Wide_String := To_Upper (Full_Name_Image
                                                               (Names (Ultimate_Type_Declaration (The_Subtype)) (1)));
      begin
         return Ultimate_Ancestor = "ADA.FINALIZATION.CONTROLLED"
           or else Ultimate_Ancestor = "ADA.FINALIZATION.LIMITED_CONTROLLED";
      end;
   end Is_Controlled;


   ----------------
   -- Is_Limited --
   ----------------

   Limited_Traits : constant array (Trait_Kinds) of Boolean
     := (  A_Limited_Trait           | A_Limited_Private_Trait
         | An_Abstract_Limited_Trait | An_Abstract_Limited_Private_Trait => True,
         others => False);
   function Is_Limited (The_Element : Asis.Element) return Boolean is
      use Asis.Definitions, Asis.Expressions;

      function Has_Limited_Components (Def : Asis.Definition) return Boolean is
         -- Expected Definition_Kinds:
         --   A_Record_Definition
         --   A_Null_Record_Definition
         --   A_Variant
         Comp_Name : Asis.Expression;
      begin
         if Definition_Kind (Def) = A_Null_Record_Definition then
            return False;
         end if;

         declare
            Components : constant Asis.Record_Component_List := Record_Components (Def);
         begin
            for I in Components'Range loop
               case Element_Kind (Components (I)) is
                  when A_Declaration =>
                     -- Can only be A_Component_Declaration
                     declare
                        CSI : constant Asis.Definition := Component_Definition_View
                                                           (Object_Declaration_View (Components (I)));
                     begin
                        if Definition_Kind (CSI) /= An_Access_Definition then
                           -- the component is not of an anonymous access type
                           Comp_Name := Subtype_Simple_Name (CSI);
                           if Expression_Kind (Comp_Name) = An_Attribute_Reference then
                              -- Limitedness is the same for 'Base and 'Class as the prefix
                              Comp_Name := Simple_Name (Prefix (Comp_Name));
                           end if;
                           if Is_Limited (Corresponding_Name_Declaration (Comp_Name)) then
                              return True;
                           end if;
                        end if;
                     end;
                  when A_Definition =>
                     case Definition_Kind (Components (I)) is
                        when A_Null_Component =>
                           null;
                        when A_Variant_Part =>
                           declare
                              The_Variants : constant Asis.Variant_List := Variants (Components (I));
                           begin
                              for J in The_Variants'Range loop
                                 if Has_Limited_Components (The_Variants (J)) then
                                    return True;
                                 end if;
                              end loop;
                           end;
                        when others =>
                           Impossible ("Wrong component definition kind", Components (I));
                     end case;
                  when others =>
                     -- We didn't ask for pragmas, and we shouldn't get An_Attribute_Definition_Clause...
                     Impossible ("Wrong component element kind", Components (I));
               end case;
            end loop;
         end;

         return False;
      end Has_Limited_Components;

      Decl : Asis.Element := The_Element;
   begin  -- Is_Limited
      loop
         case Element_Kind (Decl) is
            when A_Defining_Name =>
               Decl := Enclosing_Element (Decl);

            when A_Declaration =>
               case Declaration_Kind (Decl) is
                  when A_Variable_Declaration
                     | A_Constant_Declaration
                     | A_Deferred_Constant_Declaration
                     | A_Component_Declaration
                       =>
                     Decl := Object_Declaration_View (Decl);
                  when A_Discriminant_Specification
                     | A_Parameter_Specification
                     | A_Formal_Object_Declaration
                     | An_Object_Renaming_Declaration
                       =>
                     Decl := Declaration_Subtype_Mark (Decl);
                  when A_Private_Type_Declaration
                     | A_Formal_Type_Declaration
                       =>
                     return Limited_Traits (Trait_Kind (Decl));
                  when An_Ordinary_Type_Declaration
                     | A_Private_Extension_Declaration
                     | A_Subtype_Declaration
                       =>
                     Decl := Type_Declaration_View (Corresponding_First_Subtype (Decl));
                  when A_Task_Type_Declaration
                     | A_Protected_Type_Declaration
                       =>
                     return True;
                  when A_Single_Protected_Declaration
                     | A_Single_Task_Declaration
                     =>
                     return True;
                  when others => -- Declaration_Kind
                     return False;
               end case;

            when A_Definition =>
               case Definition_Kind (Decl) is
                  when A_Type_Definition =>
                     case Type_Kind (Decl) is
                        when A_Derived_Type_Definition =>
                           Decl := Parent_Subtype_Indication (Decl);
                        when A_Derived_Record_Extension_Definition =>
                           -- A record extension is limited iff the (ultimate) parent type is limited
                           Decl := Corresponding_Root_Type (Decl);
                        when An_Unconstrained_Array_Definition
                           | A_Constrained_Array_Definition
                             =>
                           -- The array type is limited if its components are
                           Decl := Component_Definition_View (Array_Component_Definition (Decl));
                        when A_Record_Type_Definition =>
                           if Trait_Kind (Decl) = A_Limited_Trait then
                              return True;
                           end if;
                           -- A record type is limited if any component is limited...
                           return Has_Limited_Components (Asis.Definitions.Record_Definition (Decl));
                        when A_Tagged_Record_Type_Definition =>
                           -- Tagged record types must be declared limited if they have limited components
                           --   => no need to check subcomponents
                           return Trait_Kind (Decl) = A_Limited_Trait;
                        when others => -- Type_Kind
                           return False;
                     end case;

                  when A_Private_Extension_Definition =>
                     -- A private extension is limited iff the parent type is limited
                     Decl := Ancestor_Subtype_Indication (Decl);

                  when A_Subtype_Indication =>
                     Decl := Subtype_Simple_Name (Decl);

                  when others => -- Definition_Kind
                     return False;
               end case;

            when An_Expression =>
               case Expression_Kind (Decl) is
                     when An_Identifier
                     | An_Operator_Symbol
                     | A_Character_Literal
                     | An_Enumeration_Literal
                       =>
                     Decl := Corresponding_Name_Declaration (Decl);
                  when A_Selected_Component =>
                     Decl := Selector (Decl);
                  when An_Attribute_Reference =>
                     -- T'Base or T'Class
                     Decl := Prefix (Decl);
                  when others => -- Expression_Kind
                     return False;
               end case;

            when others => -- Element_Kind
               -- Including Not_An_Element
               return False;
         end case;
      end loop;
   end Is_Limited;


   -----------------------------------------
   -- Corresponding_Full_Type_Declaration --
   -----------------------------------------

   function Corresponding_Full_Type_Declaration (Decl : Asis.Declaration) return Asis.Declaration is
      Result : Asis.Declaration := Decl;
   begin
      -- Ada 2012: Decl can be incomplete, then private, hence the loop
      while Declaration_Kind (Result) in An_Incomplete_Type_Declaration .. A_Private_Extension_Declaration loop
         Result := Corresponding_Type_Completion (Result);
      end loop;
      return Result;
   end Corresponding_Full_Type_Declaration;


   ------------------------------------------
   -- Corresponding_Derivation_Description --
   ------------------------------------------

   function Corresponding_Derivation_Description (The_Subtype : Asis.Declaration;
                                                  Privacy     : Privacy_Policy := Follow_User_Private)
                                                  return Derivation_Descriptor
   is
      use Asis.Definitions, Asis.Expressions;
      Result : Derivation_Descriptor := (Ultimate_Type    => The_Subtype,
                                         Derivation_Depth => 0,
                                         First_Constraint => Nil_Element);
      Parent : Asis.Definition;
   begin
      loop                                   --## RULE LINE OFF Simplifiable_Statements ## We prefer exit to while here
         exit when Is_Nil (Result.Ultimate_Type); -- Anonymous type...

         case Declaration_Kind (Result.Ultimate_Type) is
            when An_Ordinary_Type_Declaration =>
               exit when Type_Kind (Type_Declaration_View (Result.Ultimate_Type))
                 not in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition;

               -- NB: the only attribute possible here is 'Base, which is not interesting for us
               Parent := Parent_Subtype_Indication (Type_Declaration_View (Result.Ultimate_Type));
               Result. Ultimate_Type := Corresponding_Name_Declaration
                                         (Strip_Attributes (Subtype_Simple_Name (Parent)));
               Result.Derivation_Depth := Result.Derivation_Depth + 1;
               if Is_Nil (Result.First_Constraint) then
                  Result.First_Constraint := Subtype_Constraint (Parent);
               end if;
            when A_Task_Type_Declaration
              | A_Protected_Type_Declaration
              | A_Formal_Type_Declaration
              =>
               exit;
            when A_Private_Type_Declaration
              | A_Private_Extension_Declaration
               =>
               case Privacy is
                  when Follow_Private =>
                     Result.Ultimate_Type := Corresponding_Type_Completion (Result.Ultimate_Type);
                  when Follow_User_Private =>
                     exit when Ultimate_Origin (Result.Ultimate_Type) /= An_Application_Unit;

                     Result.Ultimate_Type := Corresponding_Type_Completion (Result.Ultimate_Type);
                  when Stop_At_Private =>
                     exit;
               end case;
            when An_Incomplete_Type_Declaration
               | A_Tagged_Incomplete_Type_Declaration
               =>
               Result.Ultimate_Type := Corresponding_Full_Type_Declaration (Result.Ultimate_Type);
            when A_Subtype_Declaration =>
               if Is_Nil (Result.First_Constraint) then
                  Result.First_Constraint := Subtype_Constraint (Type_Declaration_View (Result.Ultimate_Type));
               end if;
               Result.Ultimate_Type := Corresponding_First_Subtype (Result.Ultimate_Type);
            when others =>
               Impossible ("Corresponding_Derivation_Description: bad kind", Result.Ultimate_Type);
         end case;
      end loop;

      return Result;
   end Corresponding_Derivation_Description;


   -------------------------------
   -- Ultimate_Type_Declaration --
   -------------------------------

   function Ultimate_Type_Declaration (The_Subtype : Asis.Declaration;
                                       Privacy     : Privacy_Policy := Follow_User_Private)
                                       return Asis.Declaration
   is
   begin
      return Corresponding_Derivation_Description (The_Subtype, Privacy).Ultimate_Type;
   end Ultimate_Type_Declaration;


   ----------------------
   -- Derivation_Depth --
   ----------------------

   function Derivation_Depth (The_Subtype : Asis.Declaration;
                              Privacy     : Privacy_Policy := Follow_User_Private)
                              return Asis.ASIS_Natural
   is
   begin
      return Corresponding_Derivation_Description (The_Subtype, Privacy).Derivation_Depth;
   end Derivation_Depth;


   ------------------------------
   -- Is_Type_Declaration_Kind --
   ------------------------------

   function Is_Type_Declaration_Kind (The_Subtype : Asis.Declaration;
                                      The_Kind    : Asis.Declaration_Kinds)
                                      return Boolean
   is
      use Asis.Definitions;
      Decl : Asis.Declaration := The_Subtype;
   begin
      Decl := Corresponding_First_Subtype (Decl);
      if Type_Kind (Type_Declaration_View (Decl)) = A_Derived_Type_Definition then
         Decl := Corresponding_Root_Type (Type_Declaration_View (Decl));
      end if;
      return Declaration_Kind (Decl) = The_Kind;
   end Is_Type_Declaration_Kind;


   ---------------------------------
   -- Attribute_Clause_Expression --
   ---------------------------------

   function Attribute_Clause_Expression (Attribute : in Asis.Attribute_Kinds;
                                         Elem      : in Asis.Element)
                                         return Asis.Expression
   is
      use Asis.Clauses, Asis.Expressions;
      Decl      : Asis.Declaration;
      Good_Name : Asis.Expression;
   begin
      case Element_Kind (Elem) is
         when A_Declaration =>
            Decl := Elem;
         when A_Defining_Name =>
            Decl := Enclosing_Element (Elem);
         when An_Expression =>
            Good_Name := Elem;
            loop
               case Expression_Kind (Good_Name) is
                  when A_Selected_Component =>
                     Good_Name := Selector (Good_Name);
                     if Declaration_Kind (Corresponding_Name_Declaration (Good_Name))
                        in A_Discriminant_Specification .. A_Component_Declaration
                     then
                        -- Attribute of a record field => give up
                        return Nil_Element;
                     end if;
                  when An_Indexed_Component =>
                     -- A(I)'Size: give up
                     return Nil_Element;
                  when An_Attribute_Reference =>
                     case Attribute_Kind (Good_Name) is
                        when A_Base_Attribute =>
                           Good_Name := Prefix (Good_Name);
                        when A_Class_Attribute =>
                           -- There is no declaration for class wide type, hence no way
                           -- to retrieve the size clause.
                           -- Anyway, putting a size clause on a class-wide type seems
                           -- a strange thing to do...
                           return Nil_Element;
                        when others =>
                           Impossible ("unexpected attribute", Good_Name);
                     end case;
                  when others =>
                     exit;
               end case;
            end loop;
            Decl := Corresponding_Name_Declaration (Good_Name);
         when others =>
            Impossible ("Attribute_Clause_Expression: Incorrect parameter", Elem);
      end case;

      if Declaration_Kind (Decl) = An_Ordinary_Type_Declaration then
         Decl := Corresponding_First_Subtype (Decl);
      end if;
      if Is_Nil (Decl) then
         return Nil_Element;
      end if;

      declare
         Reprs : constant Asis.Representation_Clause_List := Corresponding_Representation_Clauses (Decl);
      begin
         for R in Reprs'Range loop
            case Representation_Clause_Kind (Reprs (R)) is
               when An_Attribute_Definition_Clause =>
                  if Attribute_Kind (Representation_Clause_Name (Reprs (R))) = Attribute then
                     return Representation_Clause_Expression (Reprs (R));
                  end if;
               when An_At_Clause =>
                  if Attribute = An_Address_Attribute then
                     return Representation_Clause_Expression (Reprs (R));
                  end if;
               when A_Record_Representation_Clause =>
                  if Attribute = An_Alignment_Attribute then
                     return Mod_Clause_Expression (Reprs (R));
                  end if;
               when others =>
                  null;
            end case;
         end loop;

         -- Clause not found
         return Nil_Element;
      end;
   end Attribute_Clause_Expression;


   ------------------------------------
   -- Corresponding_Component_Clause --
   ------------------------------------

   function Corresponding_Component_Clause (Component : in Asis.Defining_Name) return Asis.Component_Clause is
      use Asis.Clauses, Asis.Expressions;

      Parent_Decl : Asis.Declaration:= Enclosing_Element (Enclosing_Element (Component));
   begin
      -- There are various nesting levels up to the enclosing record, depending on
      -- whether we are in a variant or not... This is the easiest:
      while Element_Kind (Parent_Decl) /= A_Declaration loop
         Parent_Decl := Enclosing_Element (Parent_Decl);
      end loop;

      declare
         Repr_List : constant Asis.Representation_Clause_List
           := Corresponding_Representation_Clauses (Parent_Decl);
      begin
         for R in Repr_List'Range loop
            if Representation_Clause_Kind (Repr_List (R)) = A_Record_Representation_Clause then
               declare
                  Components : constant Asis.Component_Clause_List := Component_Clauses (Repr_List (R));
               begin
                  for C in Components'Range loop
                     if Is_Equal (Corresponding_Name_Definition (Representation_Clause_Name (Components (C))),
                                  Component)
                     then
                        -- Found the clause for this component.
                        return Components (C);
                     end if;
                  end loop;
               end;
               return Nil_Element;  -- Cannot have several record representation clauses
            end if;
         end loop;
      end;
      return Nil_Element;  -- No record representation clause found
   end Corresponding_Component_Clause;

   --------------------------------------
   -- Corresponding_Enumeration_Clause --
   --------------------------------------

   function Corresponding_Enumeration_Clause (Enumeration_Value : in Asis.Defining_Name) return Asis.Association is
      use Asis.Clauses, Asis.Expressions;

      Parent_Decl : constant Asis.Declaration := Enclosing_Element
                                                  (Enclosing_Element
                                                   (Enclosing_Element (Enumeration_Value)));
      Repr_List   : constant Asis.Representation_Clause_List := Corresponding_Representation_Clauses (Parent_Decl);
   begin
      for R in Repr_List'Range loop
         if Representation_Clause_Kind (Repr_List (R)) = An_Enumeration_Representation_Clause then
            declare
               Assoc_List : constant Asis.Association_List := Array_Component_Associations
                                                               (Representation_Clause_Expression (Repr_List (R)));
            begin
               for C in Assoc_List'Range loop
                  for I in Array_Component_Choices (Assoc_List (C))'Range loop
                     declare
                        Enumeration_Literal : constant Expression := Array_Component_Choices (Assoc_List (C)) (I);
                     begin
                        if Is_Equal (Corresponding_Name_Definition (Enumeration_Literal), Enumeration_Value) then
                           return Assoc_List (C);
                        end if;
                     end;
                  end loop;
               end loop;
            end;
            return Nil_Element;  -- Cannot have several record representation clauses
         end if;
      end loop;
      return Nil_Element;  -- No record representation clause found
   end Corresponding_Enumeration_Clause;


   -------------------
   -- Type_Category --
   -------------------

   function Type_Category (Elem               : in Asis.Element;
                           Follow_Derived     : in Boolean := False;
                           Privacy            : in Privacy_Policy := Stop_At_Private;
                           Separate_Extension : in Boolean := False) return Type_Categories
   is
      use Asis.Definitions, Asis.Expressions, Asis.Limited_Views;
      Good_Elem : Asis.Declaration;
   begin
      -- Go from (true) expressions and object declarations/definitions to their type declaration
      case Element_Kind (Elem) is
         when An_Expression =>
            Good_Elem := Simple_Name (Elem);
            if Expression_Kind (Good_Elem) = An_Identifier
              and then Declaration_Kind
                        (Corresponding_Name_Declaration
                         (Good_Elem)) in An_Ordinary_Type_Declaration ..  A_Subtype_Declaration
            then
               -- it was the name of a type, not a true expression
               Good_Elem := Corresponding_Name_Declaration (Good_Elem);
            else
               Good_Elem := Corresponding_Expression_Type_Definition (Elem);
               if Is_Nil (Good_Elem) then
                  -- Annoying special case: a task of the form "task type TT;" has no definition
                  case Expression_Kind (Elem) is
                     when An_Identifier | A_Selected_Component =>
                        -- Only these can be type names
                        declare
                           Decl : constant Asis.Declaration := Corresponding_Name_Declaration (Simple_Name (Elem));
                        begin
                           if Declaration_Kind (Decl) = A_Task_Type_Declaration then
                              return A_Task_Type;
                           else
                              return Not_A_Type;
                           end if;
                        end;
                     when others =>
                        return Not_A_Type;
                  end case;
               end if;

               -- Good_Elem is not Nil (previous "if" has returns in all branches)
               case Definition_Kind (Good_Elem) is
                  when A_Type_Definition =>
                     case Type_Kind (Good_Elem) is
                        when An_Unconstrained_Array_Definition
                           | A_Constrained_Array_Definition
                           =>
                           return An_Array_Type;
                        when others =>
                           Good_Elem := Corresponding_First_Subtype (Enclosing_Element (Good_Elem));
                     end case;
                  when A_Task_Definition =>
                     return A_Task_Type;
                  when A_Protected_Definition =>
                     return A_Protected_Type;
                  when An_Access_Definition =>
                     return An_Access_Type;
                  when A_Subtype_Indication =>
                     -- Note: we don't care about attributes ('Base or 'Range or 'Class), since they can't
                     --       change the category of the type
                     Good_Elem := Corresponding_First_Subtype (Corresponding_Name_Declaration
                                                               (Simple_Name
                                                                  (Strip_Attributes
                                                                     (Subtype_Simple_Name (Good_Elem)))));
                  when A_Component_Definition =>
                     Good_Elem := Component_Definition_View (Good_Elem);
                     if Definition_Kind (Good_Elem) = An_Access_Definition then
                        return An_Access_Type;
                     end if;
                     Good_Elem := Corresponding_First_Subtype (Corresponding_Name_Declaration
                                                               (Strip_Attributes
                                                                  (Subtype_Simple_Name (Good_Elem))));
                  when others =>
                     Good_Elem := Corresponding_First_Subtype (Enclosing_Element (Good_Elem));
               end case;
            end if;
         when A_Declaration =>
            case Declaration_Kind (Elem) is
               when A_Variable_Declaration
                  | A_Constant_Declaration
                  | A_Deferred_Constant_Declaration
                  =>
                  Good_Elem := Object_Declaration_View (Elem);
                  case Definition_Kind (Good_Elem) is
                     when A_Type_Definition =>
                        if Type_Kind (Good_Elem) /= A_Constrained_Array_Definition then
                           Impossible ("Type_Category: anonymous type not array", Good_Elem);
                        end if;
                        return An_Array_Type;
                     when An_Access_Definition =>
                        return An_Access_Type;
                     when others =>
                        null;
                  end case;
                  Good_Elem := Subtype_Simple_Name (Good_Elem);
                  case Attribute_Kind (Good_Elem) is
                     when Not_An_Attribute =>
                        Good_Elem := Corresponding_Name_Declaration (Good_Elem);
                     when A_Class_Attribute =>
                        return A_Tagged_Type;
                     when A_Base_Attribute =>
                        Good_Elem := Corresponding_Name_Declaration (Simple_Name (Prefix (Good_Elem)));
                     when others =>
                        Impossible ("Type category: attribute should be type", Good_Elem);
                  end case;
               when A_Component_Declaration =>
                  Good_Elem := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                               (Component_Definition_View
                                                                (Object_Declaration_View (Elem))));
               when A_Discriminant_Specification =>
                  -- Object_Declaration_View returns a name. If attribute, cannot be 'Class, and we ignore 'Base
                  Good_Elem := Corresponding_Name_Declaration (Strip_Attributes
                                                               (Simple_Name (Object_Declaration_View (Elem))));

               when An_Ordinary_Type_Declaration
                  | A_Task_Type_Declaration
                  | A_Protected_Type_Declaration
                  | A_Private_Type_Declaration
                  | A_Private_Extension_Declaration
                  | A_Subtype_Declaration
                  | A_Formal_Type_Declaration
                    =>
                  Good_Elem := Elem;
               when others =>
                  return Not_A_Type;
            end case;
         when A_Definition =>
            case Definition_Kind (Elem) is
               when A_Type_Definition =>
                  if Type_Kind (Elem) in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition then
                     return An_Array_Type;
                  end if;
                  Good_Elem := Enclosing_Element (Elem);
               when A_Task_Definition =>
                  return A_Task_Type;
               when A_Protected_Definition =>
                  return A_Protected_Type;
               when An_Access_Definition =>
                  return An_Access_Type;
               when A_Subtype_Indication =>
                  -- Note: we don't care about attributes ('Base or 'Range or 'Class), since they can't
                  --       change the category of the type
                  Good_Elem := Corresponding_First_Subtype (Corresponding_Name_Declaration
                                                            (Simple_Name
                                                               (Strip_Attributes
                                                                  (Subtype_Simple_Name (Elem)))));
               when A_Private_Type_Definition
                  | A_Private_Extension_Definition
                  | A_Formal_Type_Definition
                  =>
                  Good_Elem := Enclosing_Element (Elem);
               when others =>
                  Impossible ("Type category: wrong definition_kind", Elem);
            end case;
         when A_Defining_Name =>
            Good_Elem := Enclosing_Element (Elem);
            if Declaration_Kind (Good_Elem) = A_Deferred_Constant_Declaration then
               Good_Elem := Corresponding_Constant_Declaration (Good_Elem);
            end if;
            if Declaration_Kind (Good_Elem) in A_Variable_Declaration .. A_Single_Protected_Declaration then
               case Declaration_Kind (Good_Elem) is
                  when A_Variable_Declaration | A_Constant_Declaration =>
                     Good_Elem := Object_Declaration_View (Good_Elem);
                     if Definition_Kind (Good_Elem) = An_Access_Definition then
                        return An_Access_Type;
                     end if;
                     if Type_Kind (Good_Elem) in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition
                     then
                        return An_Array_Type;
                     end if;
                     Good_Elem := Corresponding_Name_Declaration (Simple_Name
                                                                  (Strip_Attributes
                                                                   (Subtype_Simple_Name (Good_Elem))));
                  when A_Deferred_Constant_Declaration =>
                     Impossible ("A_Deferred_Constant_Declaration", Good_Elem);
                  when A_Single_Task_Declaration =>
                     return A_Task_Type;
                  when A_Single_Protected_Declaration =>
                     return A_Protected_Type;
                  when others =>
                     -- Assume a type
                     null;
               end case;
            end if;
         when others =>
            return Not_A_Type;
      end case;

      -- At this point, Good_Elem is a (formal) type declaration, or Nil_Element in some weird cases
      if Is_Nil (Good_Elem) then
         -- Special case: not a good ol' type.
         -- It could be universal fixed if the expression is a fixed point multiply/divide
         if Expression_Kind (Elem) /= A_Function_Call
           or else Operator_Kind (Simple_Name (Prefix (Elem))) not in A_Multiply_Operator .. A_Divide_Operator
         then
            return Not_A_Type;
         end if;

         -- Here, we have "*" or "/"
         declare
            Params : constant Asis.Association_List := Function_Call_Parameters (Elem);
         begin
            if Type_Category (Actual_Parameter (Params (1)), Follow_Derived => True) = A_Fixed_Point_Type
              and then Type_Category (Actual_Parameter (Params (2)), Follow_Derived => True) = A_Fixed_Point_Type
            then
               -- Both operands are fixed => Assume the result is fixed (remember we know it is
               -- a predefined operator).
               return A_Fixed_Point_Type;
            else
               return Not_A_Type;
            end if;
         end;
      end if;

      -- At this point, Good_Elem is a (formal) type declaration
      -- If an incomplete type, go to full type declaration   #0000041
      if Declaration_Kind (Good_Elem) in An_Incomplete_Type_Declaration .. A_Tagged_Incomplete_Type_Declaration then
         if Is_From_Limited_View (Good_Elem) then
            Good_Elem := Get_Nonlimited_View (Good_Elem);
         end if;
         Good_Elem := Corresponding_Full_Type_Declaration (Good_Elem);
      end if;
      Good_Elem := Corresponding_First_Subtype (Good_Elem);

      loop -- because of derived and incomplete types
         case Declaration_Kind (Good_Elem) is
            when An_Ordinary_Type_Declaration =>
               case Type_Kind (Type_Declaration_View (Good_Elem)) is
                  when A_Derived_Type_Definition =>
                     if not Follow_Derived then
                        return A_Derived_Type;
                     end if;
                     Good_Elem := Subtype_Simple_Name (Parent_Subtype_Indication
                                                       (Type_Declaration_View (Good_Elem)));
                     case Attribute_Kind (Good_Elem) is
                        when Not_An_Attribute =>
                           null;
                        when A_Class_Attribute =>
                           return A_Tagged_Type;
                        when A_Base_Attribute =>
                           Good_Elem := Simple_Name (Prefix (Good_Elem));
                        when others =>
                           Impossible ("Bad attribute in Type_Category", Good_Elem);
                     end case;
                     Good_Elem := Corresponding_First_Subtype (Corresponding_Name_Declaration (Good_Elem));
                  when An_Enumeration_Type_Definition =>
                     return An_Enumeration_Type;
                  when A_Signed_Integer_Type_Definition =>
                     return A_Signed_Integer_Type;
                  when A_Modular_Type_Definition =>
                     return A_Modular_Type;
                  when A_Root_Type_Definition =>
                     case Root_Type_Kind (Type_Declaration_View (Good_Elem)) is
                        when Not_A_Root_Type_Definition =>
                           Impossible ("Not a root type", Good_Elem);
                        when A_Root_Integer_Definition | A_Universal_Integer_Definition =>
                           return A_Signed_Integer_Type;
                        when A_Root_Real_Definition | A_Universal_Real_Definition =>
                           return A_Floating_Point_Type;
                        when A_Universal_Fixed_Definition =>
                           return A_Fixed_Point_Type;
                     end case;
                  when A_Floating_Point_Definition =>
                     return A_Floating_Point_Type;
                  when An_Ordinary_Fixed_Point_Definition =>
                     return A_Fixed_Point_Type;
                  when A_Decimal_Fixed_Point_Definition =>
                     return A_Fixed_Point_Type;
                  when An_Unconstrained_Array_Definition
                     | A_Constrained_Array_Definition
                       =>
                        return An_Array_Type;
                  when A_Record_Type_Definition =>
                     return A_Record_Type;
                  when A_Tagged_Record_Type_Definition =>
                     return A_Tagged_Type;
                  when A_Derived_Record_Extension_Definition =>
                     if Separate_Extension then
                        return An_Extended_Tagged_Type;
                     else
                        return A_Tagged_Type;
                     end if;
                  when An_Access_Type_Definition =>
                     return An_Access_Type;
                  when others =>
                     return Not_A_Type;
               end case;
            when A_Formal_Type_Declaration =>
               case Formal_Type_Kind (Type_Declaration_View (Good_Elem)) is
                  when  A_Formal_Private_Type_Definition =>
                     return A_Private_Type;
                  when A_Formal_Tagged_Private_Type_Definition =>
                     return A_Tagged_Type;
                  when A_Formal_Derived_Type_Definition =>
                     if not Follow_Derived then
                        return A_Derived_Type;
                     end if;
                     Good_Elem := Subtype_Simple_Name (Type_Declaration_View (Good_Elem));
                     case Attribute_Kind (Good_Elem) is
                        when Not_An_Attribute =>
                           null;
                        when A_Class_Attribute =>
                           return A_Tagged_Type;
                        when A_Base_Attribute =>
                           Good_Elem := Simple_Name (Prefix (Good_Elem));
                        when others =>
                           Impossible ("Bad attribute in Type_Category", Good_Elem);
                     end case;
                     Good_Elem := Corresponding_First_Subtype (Corresponding_Name_Declaration (Good_Elem));
                  when A_Formal_Discrete_Type_Definition =>
                     return An_Enumeration_Type;
                  when A_Formal_Signed_Integer_Type_Definition =>
                     return A_Signed_Integer_Type;
                  when A_Formal_Modular_Type_Definition =>
                     return A_Modular_Type;
                  when A_Formal_Floating_Point_Definition =>
                     return A_Floating_Point_Type;
                  when A_Formal_Ordinary_Fixed_Point_Definition =>
                     return A_Fixed_Point_Type;
                  when A_Formal_Decimal_Fixed_Point_Definition =>
                     return A_Fixed_Point_Type;
                  when A_Formal_Unconstrained_Array_Definition
                     | A_Formal_Constrained_Array_Definition
                       =>
                     return An_Array_Type;
                  when A_Formal_Access_Type_Definition =>
                     return An_Access_Type;
                  when others =>
                     return Not_A_Type;
               end case;
            when A_Task_Type_Declaration =>
               return A_Task_Type;
            when A_Protected_Type_Declaration =>
               return A_Protected_Type;
            when An_Incomplete_Type_Declaration | A_Tagged_Incomplete_Type_Declaration =>
               -- Note that tagged incomplete can correspond to a tagged declaration or to
               -- an extension => we must also go to the full type
               Good_Elem := Corresponding_Full_Type_Declaration (Good_Elem);
            when A_Private_Type_Declaration =>
               case Privacy is
                  when Follow_Private =>
                     Good_Elem := Corresponding_Full_Type_Declaration (Good_Elem);
                  when Follow_User_Private =>
                     if Ultimate_Origin (Good_Elem) = An_Application_Unit then --##rule line off Simplifiable_statements
                        Good_Elem := Corresponding_Full_Type_Declaration (Good_Elem);
                     else
                        return A_Private_Type;
                     end if;
                  when Stop_At_Private =>
                     return A_Private_Type;
               end case;
            when A_Private_Extension_Declaration =>
               if Separate_Extension then
                  return An_Extended_Tagged_Type;
               else
                  return A_Tagged_Type;
               end if;
            when others =>
               return Not_A_Type;
         end case;
      end loop;
   end Type_Category;

   ------------------------------------
   -- Contains_Type_Declaration_Kind --
   ------------------------------------

   function Contains_Type_Declaration_Kind (The_Subtype : Asis.Declaration;
                                            The_Kind    : Asis.Declaration_Kinds;
                                            The_Type    : Asis.Type_Kinds := Asis.Not_A_Type_Definition) return Boolean
   is
      type Null_State is null record;
      procedure Pre_Declaration_Kind (Element : in     Asis.Definition;
                                      Control : in out Asis.Traverse_Control;
                                      State   : in out Null_State;
                                      Depth   : in     Asis.ASIS_Positive)
      is
         use Asis.Expressions;
         Decl : Asis.Declaration;
      begin
         case Definition_Kind (Element) is
            when Not_A_Definition =>
               -- Special case: Task type without a definition
               if The_Kind = A_Task_Type_Declaration then
                  Control := Terminate_Immediately;
               end if;
            when An_Access_Definition =>
               if The_Kind = An_Ordinary_Type_Declaration and then The_Type = An_Access_Type_Definition then
                  Control := Terminate_Immediately;
               end if;
            when A_Subtype_Indication =>
               -- Check first subtype instead
               Pre_Declaration_Kind (Type_Declaration_View
                                     (Corresponding_First_Subtype
                                      (Corresponding_Name_Declaration
                                       (Strip_Attributes
                                        (Subtype_Simple_Name (Element))))),
                                     Control,
                                     State,
                                     Depth);
            when others =>
               Decl := Enclosing_Element (Element);
               if Is_Type_Declaration_Kind (Decl, The_Kind) then
                  if        The_Type = Not_A_Type_Definition
                    or else The_Type = Type_Kind (Element)
                  then
                     Control := Terminate_Immediately;
                  end if;
               end if;
         end case;
      end Pre_Declaration_Kind;

      procedure Traverse_For_Declaration_Kind is new Traverse_Data_Structure (Null_State,
                                                                              Pre_Declaration_Kind);
      Control : Traverse_Control := Continue;
      State   : Null_State;
   begin  -- Contains_Type_Declaration_Kind
      Traverse_For_Declaration_Kind (The_Subtype, Control, State);
      return Control = Terminate_Immediately;    -- We terminate immediately iff the subtype is found
   end Contains_Type_Declaration_Kind;


   -----------------------------
   -- Traverse_Data_Structure --
   -----------------------------

   procedure Traverse_Data_Structure (Element : in     Asis.Element;
                                      Control : in out Asis.Traverse_Control;
                                      State   : in out State_Information)
   is

      procedure Counting_Traverse (Elem_Decl        : in     Asis.Declaration;
                                   Elem_Def         : in     Asis.Definition;
                                   Counting_Control : in out Asis.Traverse_Control;
                                   Counting_State   : in out State_Information;
                                   Depth            : in     Asis.Asis_Positive)
        -- Elem_Decl and Elem_Def are corresponding declaration and definition
        -- We have to pass both, because anonymous types have a definition without a declaration,
        -- and simple tasks (task T;) have a declaration without a definition
      is
         -- This is the real traversal, the enclosing one is just used to initialize Depth.
         use Asis.Definitions, Asis.Expressions;
         Good_Decl : Asis.Declaration;
         Good_Def  : Asis.Definition;
         Comp_Decl : Asis.Declaration;
         Comp_Def  : Asis.Definition;

         procedure Traverse_Components_List (Components : Asis.Record_Component_List) is
            Name : Asis.Expression;
            Def  : Asis.Definition;
            Decl : Asis.Declaration;
         begin
            for I in Components'Range loop
               case Element_Kind (Components (I)) is
                  when A_Declaration =>
                     -- A_Component_Declaration
                     Def := Component_Definition_View (Object_Declaration_View (Components (I)));
                     if Definition_Kind (Def) = An_Access_Definition then
                        Counting_Traverse (Nil_Element, Def, Counting_Control, Counting_State, Depth + 1);
                     else
                        Name := Subtype_Simple_Name (Def);
                        if Expression_Kind (Name) = An_Attribute_Reference then
                           -- A record component can't be 'Class, must be 'Base
                           Name := Simple_Name (Prefix (Name));
                        end if;
                        Decl := Corresponding_Name_Declaration (Name);
                        -- Def is nil for a task declaration without task definition (i.e. task T; )
                        Counting_Traverse (Decl, Type_Declaration_View(Decl), Counting_Control, Counting_State, Depth + 1);
                     end if;
                     case Counting_Control is
                        when Continue =>
                           null;
                        when Abandon_Children =>
                           Impossible ("Traverse_Data_Structure: Abandon_Children (3)", Name);
                        when Abandon_Siblings | Terminate_Immediately =>
                           return;
                     end case;
                  when A_Definition =>
                     if Definition_Kind (Components (I)) = A_Variant_Part then -- else it is A_Null_Component
                        declare
                           V_List : constant Asis.Variant_List := Variants (Components (I));
                        begin
                           for V in V_List'Range loop
                              Traverse_Components_List (Record_Components (V_List (V)));
                              case Counting_Control is
                                 when Continue =>
                                    null;
                                 when Abandon_Children =>
                                    Impossible ("Traverse_Data_Structure: Abandon_Children", Components (I));
                                 when Abandon_Siblings | Terminate_Immediately =>
                                    return;
                              end case;
                           end loop;
                        end;
                     end if;
                  when others =>
                     -- pragma, clause
                     null;
               end case;
            end loop;
         end Traverse_Components_List;
      begin  -- Counting_Traverse
         -- Perform Operation on the definition itself
         Pre_Operation (Elem_Def, Counting_Control, Counting_State, Depth);
         case Counting_Control is
            when Continue =>
               null;
            when Abandon_Children =>
               Counting_Control := Continue;
               Post_Operation (Elem_Def, Counting_Control, Counting_State, Depth);
               if Counting_Control = Abandon_Children then
                  Counting_Control := Continue;
               end if;
               return;
            when Abandon_Siblings | Terminate_Immediately =>
               -- Keep same Control for next level
               -- According to the doc for Traverse_Element,
               -- Post_Operation is not executed if Pre returns Abandon_Siblings
               return;
         end case;

         -- To recurse ignoring privacy, get rid of (possible) incomplete declarations
         -- Go to first named subtype, and eliminate derivations (we are only interested in the actual structure)
         Good_Def  := Elem_Def;
         Good_Decl := Elem_Decl;
         loop
            case Declaration_Kind (Good_Decl) is
               when An_Ordinary_Type_Declaration =>
                  case Type_Kind (Good_Def) is
                     when A_Derived_Type_Definition =>
                        Good_Decl := Corresponding_Root_Type (Good_Def);
                        Good_Def  := Type_Declaration_View (Good_Decl);
                     when others =>
                        exit;
                  end case;
               when An_Incomplete_Type_Declaration
                  | A_Tagged_Incomplete_Type_Declaration
                  | A_Private_Type_Declaration
                  | A_Private_Extension_Declaration
                  =>
                  Good_Decl := Corresponding_Full_Type_Declaration (Good_Decl);
                  Good_Def  := Type_Declaration_View (Good_Decl);
               when A_Subtype_Declaration =>
                  Good_Decl := Corresponding_First_Subtype (Good_Decl);
                  Good_Def  := Type_Declaration_View (Good_Decl);
               when others =>
                  exit;
            end case;
         end loop;

         -- Now, recurse into children
         -- Discriminants
         case Declaration_Kind (Good_Decl) is
            when A_Type_Declaration | A_Formal_Type_Declaration =>
               declare
                  Discr_Part : constant Asis.Definition := Discriminant_Part (Good_Decl);
               begin
                  if not Is_Nil (Discr_Part) and then Definition_Kind (Discr_Part) /= An_Unknown_Discriminant_Part then
                     declare
                        Discrs : constant Asis.Discriminant_Specification_List := Discriminants (Discr_Part);
                     begin
                        for D in Discrs'Range loop
                           Comp_Def := Object_Declaration_View (Discrs (D));
                           if Definition_Kind (Comp_Def) = An_Access_Definition then
                              -- access discriminant
                              Counting_Traverse (Nil_Element, Comp_Def, Counting_Control, Counting_State, Depth + 1);
                           else
                              Comp_Decl := Corresponding_Name_Declaration (Simple_Name (Strip_Attributes (Comp_Def)));
                              Counting_Traverse (Comp_Decl,
                                                 Type_Declaration_View (Comp_Decl),
                                                 Counting_Control,
                                                 Counting_State,
                                                 Depth + 1);
                           end if;
                           case Counting_Control is
                              when Continue =>
                                 null;
                              when Abandon_Children =>
                                 Impossible ("Traverse_Data_Structure: Abandon_Children (1)", Discrs (D));
                              when Abandon_Siblings =>
                                 Counting_Control := Continue;
                                 return;
                              when Terminate_Immediately =>
                                 return;
                           end case;
                        end loop;
                     end;
                  end if;
               end;
            when others =>
               null;
         end case;

         -- Other components
         case Type_Kind (Good_Def) is
            when An_Unconstrained_Array_Definition | A_Constrained_Array_Definition =>
               -- Get rid of 'Base if any (cannot be 'Class)
               Comp_Decl := Corresponding_Name_Declaration (Strip_Attributes
                                                            (Subtype_Simple_Name
                                                             (Component_Definition_View
                                                              (Array_Component_Definition (Good_Def)))));
               Counting_Traverse (Comp_Decl, Type_Declaration_View (Comp_Decl), Counting_Control, Counting_State, Depth + 1);
               case Counting_Control is
                  when Continue =>
                     null;
                  when Abandon_Children =>
                     Impossible ("Traverse_Data_Structure: Abandon_Children (2)", Good_Def);
                  when Abandon_Siblings =>
                     Counting_Control := Continue;
                     return;
                  when Terminate_Immediately =>
                     return;
               end case;
            when A_Record_Type_Definition | A_Tagged_Record_Type_Definition =>
               if Definition_Kind (Asis.Definitions.Record_Definition (Good_Def)) /= A_Null_Record_Definition then
                  Traverse_Components_List (Record_Components (Asis.Definitions.Record_Definition (Good_Def)));
                  case Counting_Control is
                     when Continue =>
                        null;
                     when Abandon_Children =>
                        Impossible ("Traverse_Data_Structure: Abandon_Children (1)", Good_Def);
                     when Abandon_Siblings =>
                        Counting_Control := Continue;
                        return;
                     when Terminate_Immediately =>
                        return;
                  end case;
               end if;
            when A_Derived_Record_Extension_Definition =>
               Comp_Decl := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                            (Parent_Subtype_Indication (Good_Def)));
               Counting_Traverse (Comp_Decl, Type_Declaration_View (Comp_Decl), Counting_Control, Counting_State, Depth + 1);
               case Counting_Control is
                  when Continue =>
                     if Definition_Kind (Asis.Definitions.Record_Definition (Good_Def)) /= A_Null_Record_Definition then
                        Traverse_Components_List (Record_Components (Asis.Definitions.Record_Definition (Good_Def)));
                        case Counting_Control is
                           when Continue =>
                              null;
                           when Abandon_Children =>
                              Impossible ("Traverse_Data_Structure: Abandon_Children (4)", Good_Def);
                           when Abandon_Siblings =>
                              Counting_Control := Continue;
                              return;
                           when Terminate_Immediately =>
                              return;
                        end case;
                     end if;
                  when Abandon_Children =>
                     Impossible ("Traverse_Data_Structure: Abandon_Children (5)", Good_Def);
                  when Abandon_Siblings =>
                     Counting_Control := Continue;
                     return;
                  when Terminate_Immediately =>
                     return;
               end case;
            when others =>
               null;
         end case;

         Post_Operation (Elem_Def, Counting_Control, Counting_State, Depth);
         if Counting_Control /= Terminate_Immediately then
            Counting_Control := Continue;
         end if;
      end Counting_Traverse;

      Decl : Asis.Declaration;
   begin -- Traverse_Data_Structure
      case Element_Kind (Element) is
         when A_Declaration =>
            Counting_Traverse (Element, Type_Declaration_View (Element), Control, State, 1);
         when A_Definition =>
            Decl := Enclosing_Element (Element);
            if Declaration_Kind (Decl) not in A_Type_Declaration then
               -- Definition comes from an anonymous type
               Counting_Traverse (Nil_Element, Element, Control, State, 1);
            else
               Counting_Traverse (Decl, Element, Control, State, 1);
            end if;
         when others =>
            Impossible ("Traverse_Data_Structure: bad Element", Element);
      end case;
   end Traverse_Data_Structure;


   ----------------------------
   -- Discriminant_Part_Kind --
   ----------------------------

   function Discriminant_Part_Kind (Elem : Asis.Element) return Discriminant_Part_Kinds is
      use Asis.Definitions;
      Discr : Asis.Definition;
   begin
      if Element_Kind (Elem) = A_Declaration then
         Discr := Discriminant_Part (Elem);
      else
         Discr := Elem;
      end if;

      if Is_Nil (Discr) then
         return No_Discriminant_Part;
      elsif Definition_Kind (Discr) = Asis.An_Unknown_Discriminant_Part then
         return An_Unknown_Discriminant_Part;
      elsif Is_Nil (Initialization_Expression (Discriminants (Discr) (1))) then
         return A_Nondefaulted_Discriminant_Part;
      else
         return A_Defaulted_Discriminant_Part;
      end if;
   end Discriminant_Part_Kind;

   -------------------
   -- Profile_Image --
   -------------------

   function Profile_Image (The_Name : Asis.Element; With_Profile : Boolean := True) return Wide_String is
      Decl_Name : Asis.Defining_Name;

      ----------------
      -- Entry_Name --
      ----------------
      function Build_Names (The_List : Profile_Table) return Wide_String;  -- Forward

      function Entry_Name (The_Entry : Profile_Entry) return Wide_String is
         function Attributed_Name return Wide_String is
            Name_Image : constant Wide_String := Full_Name_Image (The_Entry.Name, With_Profile);
         begin
            case The_Entry.Attribute is
               when None =>
                  return Name_Image;
               when Base =>
                  return Name_Image & "'BASE";
               when Class =>
                  return Name_Image & "'CLASS";
            end case;
         end Attributed_Name;

      begin  -- Entry_Name
         -- 'O' for object, 'P' for procedure, 'F' for function
         case The_Entry.Access_Form is
            when Not_An_Access_Definition =>
               return Attributed_Name;
            when An_Anonymous_Access_To_Variable | An_Anonymous_Access_To_Constant =>
               return "*O" & Attributed_Name;
            when An_Anonymous_Access_To_Procedure | An_Anonymous_Access_To_Protected_Procedure =>
               return "*P{" & Build_Names (The_Entry.Anon_Profile.Formals) & '}';
            when An_Anonymous_Access_To_Function | An_Anonymous_Access_To_Protected_Function =>
               return "*F{" & Build_Names (The_Entry.Anon_Profile.Formals)
                      & ':' & Entry_Name  (The_Entry.Anon_Profile.Result_Type) & '}';
         end case;
      end Entry_Name;

      function Build_Names (The_List : Profile_Table) return Wide_String is
      begin
         if The_List'Length = 0 then
            return "";
         elsif The_List'Length = 1 then
            return Entry_Name (The_List (The_List'First));
         else
            return Entry_Name (The_List (The_List'First)) &
              ';' &
              Build_Names (The_List (The_List'First+1..The_List'Last));
         end if;
      end Build_Names;

      use Asis.Expressions;
   begin   -- Profile_Image
      if not Is_Callable_Construct (The_Name) then
         return "";
      end if;

      if Element_Kind (The_Name) = A_Defining_Name then
         Decl_Name := Enclosing_Element (The_Name);
         while Element_Kind (Decl_Name) = A_Defining_Name loop
            -- Case of a Defining_Expanded_Name
            Decl_Name := Enclosing_Element (Decl_Name);
         end loop;
      else
         Decl_Name := Corresponding_Name_Declaration (Simple_Name (The_Name));
      end if;

      declare
         Profile_Names : constant Profile_Descriptor := Types_Profile (Decl_Name);
      begin
         if Is_Nil (Profile_Names.Result_Type.Name) then
            -- A procedure (or entry...)
            return '{' & Build_Names (Profile_Names.Formals) & '}';
         else
            -- A function
            return '{' & Build_Names (Profile_Names.Formals) & ':' &
            Entry_Name (Profile_Names.Result_Type) & '}';
         end if;
      end;
   end Profile_Image;

   ----------------------------------------------
   -- Corresponding_Expression_Type_Definition --
   ----------------------------------------------

   function Corresponding_Expression_Type_Definition (The_Element : Asis.Expression) return Asis.Definition is
      use Asis.Definitions, Asis.Expressions;
      Local_Elem : Asis.Element := A4G_Bugs.Corresponding_Expression_Type (The_Element);
      Def        : Asis.Definition;
   begin
      if Is_Nil (Local_Elem) then
         -- Normally, we are called only with "true" expressions, which are supposed to have a type
         -- However, The_Element can be the name of a protected type or a task type from its own body
         -- designating the current instance. A4G returns Nil_Element for Corresponding_Expression_Type
         -- in that case.
         -- Incidentally, this will make Corresponding_Expression_Type_Definition work for any type
         -- To be checked if replaced by the equivalent ASIS05 query
         --
         -- The case of Nil_Element returned by expressions of an anonymous access type is handled
         -- later

         -- Special case: funny identifiers from pragmas
         if Association_Kind (Enclosing_Element (The_Element)) = A_Pragma_Argument_Association then
            return Nil_Element;
         end if;

         case Expression_Kind (The_Element) is
            when An_Identifier | A_Selected_Component =>
               -- Only these can be type names
               declare
                  Decl : constant Asis.Declaration := Corresponding_Name_Declaration (Simple_Name (The_Element));
               begin
                  case Declaration_Kind (Decl) is
                     when A_Type_Declaration | A_Subtype_Declaration | A_Formal_Type_Declaration =>
                        Local_Elem := Decl;
                     when others =>
                        null;
                  end case;
               end;
            when An_Attribute_Reference =>
               case Attribute_Kind (The_Element) is
                  when A_Base_Attribute | A_Class_Attribute =>
                     -- We ignore 'Base or 'Class for the purpose of the type definition
                     return Corresponding_Expression_Type_Definition (Strip_Attributes (The_Element));
                  when others =>
                     -- 'Read, 'Write and friends, because these are procedures
                     -- (for attributes that are functions, Corresponding_Expression_Type is not Nil)
                     return Nil_Element;
               end case;
            when others =>
               null;
         end case;
      end if;

      -- We want a true definition, therefore we have to look through private types (and incomplete types of course)
      Local_Elem := Corresponding_Full_Type_Declaration (Local_Elem);
      if not Is_Nil (Local_Elem) then
         -- Normal case, we have a type declaration
         return Type_Declaration_View (Corresponding_First_Subtype (Local_Elem));
      end if;

      -- No type declaration, see if we can retrieve the definition of an anonymous type

      -- Loop till we find an appropriate name
      Local_Elem := The_Element;
      loop
         case Expression_Kind (Local_Elem) is
            when A_Null_Literal
               | An_Allocation_From_Subtype
               | An_Allocation_From_Qualified_Expression
               =>
               -- Get the type from the expected type
               Local_Elem := Enclosing_Element (Local_Elem);
               if Statement_Kind (Local_Elem) = An_Assignment_Statement then
                  Local_Elem := Assignment_Variable_Name (Local_Elem);
               elsif Declaration_Kind (Local_Elem) = A_Parameter_Specification then
                  -- The expression was the default expression of a parameter
                  Local_Elem := Names (Local_Elem) (1);
                  exit;
               else
                  case Association_Kind (Local_Elem) is
                     --when A_Discriminant_Association =>
                     --when A_Record_Component_Association =>
                     --when An_Array_Component_Association =>
                     when A_Parameter_Association | A_Generic_Association =>
                        Local_Elem := Formal_Name (Local_Elem);
                        if Is_Nil (Local_Elem) then
                           -- Too bad, the association is from a predefined operator
                           -- without an implicit declaration => No formal name available
                           return Nil_Element;
                        end if;
                        exit;
                     when others =>
                        return Nil_Element;
                        -- TBSL: as long as other associations are unimplemented
                        --Impossible ("Corresponding_Expression_Type_Definition: unexpected association", Local_Elem);
                  end case;
               end if;
            when A_Selected_Component =>
               Local_Elem := Selector (Local_Elem);
            when An_Identifier =>
               exit;
            when A_Slice =>
               -- Short of a better idea, take the definition from the sliced object
               -- Note that the bounds will therefore not be the ones of the slice
               Local_Elem  := Prefix (Local_Elem);
            when An_Indexed_Component =>
               -- 2005 Joy! Now, array components can be of an anonymous (access) type.
               Def := Corresponding_Expression_Type_Definition (Prefix (Local_Elem));
               if Is_Nil (Def) then
                  -- This is the case if the indexing was for an entry family,
                  -- there is really no type in sight
                  return Nil_Element;
               end if;
               if Type_Kind (Def)
                    not in An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition
                 and then Formal_Type_Kind (Def)
                    not in A_Formal_Unconstrained_Array_Definition .. A_Formal_Constrained_Array_Definition
               then
                  -- Ada 2012: the prefix maybe anything for which Constant_Indexing or Variable_Indexing is specified
                  -- TBSL: give up for now
                  return Nil_Element;
               end if;
               return Component_Definition_View (Array_Component_Definition (Def));
            when A_Function_Call =>
               Def := Corresponding_Name_Declaration (Simple_Name (Prefix (Local_Elem)));
               if Declaration_Kind (Def) = A_Function_Instantiation then
                  Def := Corresponding_Declaration (Def);
               end if;
               Def := Result_Profile (Def);
               if Definition_Kind (Def) = An_Access_Definition then -- ASIS 2005
                  -- Result type is an anonymous access type
                  return Def;
               else
                  -- An identifier, selected name, or attribute
                  return Type_Declaration_View (Corresponding_Name_Declaration
                                                (Simple_Name
                                                   (Strip_Attributes (Def))));
               end if;
            when others =>
               -- TBSL This unfortunately covers the case of an aggregate of an anonymous array type, like in:
               --   Tab : array (1..10) of Integer := (1..10 => 0);
               -- No way to solve this in sight...
               return Nil_Element;
         end case;
      end loop;

      if Element_Kind (Local_Elem) = A_Defining_Name then
         Local_Elem := Enclosing_Element (Local_Elem);
      else
         Local_Elem := Corresponding_Name_Declaration (Local_Elem);
      end if;

      case Declaration_Kind (Local_Elem) is
         when A_Variable_Declaration
            | A_Constant_Declaration
            | A_Deferred_Constant_Declaration
              =>
            Def := Object_Declaration_View (Local_Elem);
            case Definition_Kind (Def) is
               when A_Type_Definition =>
                  -- This can only be an anonymous array => we have the definition
                  return Def;
               when An_Access_Definition =>   -- ASIS 2005
                 -- Anonymous access type
                  return Def;
               when others =>
                  return Type_Declaration_View (Corresponding_Name_Declaration
                                                (Simple_Name
                                                 (Strip_Attributes   -- Ignore 'Base and 'Class if any
                                                  (Subtype_Simple_Name (Def)))));
            end case;
         when A_Discriminant_Specification
            | A_Parameter_Specification
            =>
            Def := Object_Declaration_View (Local_Elem);
            if Definition_Kind (Def) = An_Access_Definition then
               return Def;
            end if;
            return Type_Declaration_View (Corresponding_Name_Declaration (Strip_Attributes (Simple_Name (Def))));
         when A_Formal_Object_Declaration =>
            if Definition_Kind (Object_Declaration_View (Local_Elem)) = An_Access_Definition then
               -- Must be an anonymous formal type
               return Object_Declaration_View (Local_Elem);
            else
               return  Type_Declaration_View (Corresponding_Name_Declaration
                                              (Declaration_Subtype_Mark (Local_Elem)));
            end if;
         when A_Component_Declaration =>
            Def := Component_Definition_View (Object_Declaration_View (Local_Elem));   -- ASIS 2005
            if Definition_Kind (Def) = An_Access_Definition then -- ASIS 2005
               -- A component whose type is an anonymous access type
               return Def;
            else
               return Type_Declaration_View (Corresponding_Name_Declaration (Strip_Attributes (Simple_Name (Def))));
            end if;
         when A_Single_Protected_Declaration
            | A_Single_Task_Declaration
              =>
            return Object_Declaration_View (Local_Elem);
         when An_Object_Renaming_Declaration =>
            return Corresponding_Expression_Type_Definition (Renamed_Entity (Local_Elem));
         when others =>
            return Nil_Element;
      end case;

   end Corresponding_Expression_Type_Definition;


   ------------------------------
   -- Corresponding_Components --
   ------------------------------

   function Corresponding_Components (The_Element : Asis.Element) return Asis.Record_Component_List is
      use Asis.Definitions, Asis.Expressions;

      Subtype_Indication : Asis.Subtype_Indication;
      Name_Decl          : Asis.Declaration;
   begin
      case Element_Kind (The_Element) is
         when A_Defining_Name =>
            Name_Decl := Enclosing_Element (The_Element);
         when An_Expression =>
            if Expression_Kind (Simple_Name (The_Element)) /= An_Identifier then
               return Nil_Element_List;
            end if;
            Name_Decl := Corresponding_Name_Declaration (Simple_Name (The_Element));
         when others =>
            return Nil_Element_List;
      end case;

      case Declaration_Kind (Name_Decl) is
         when A_Discriminant_Specification
            | A_Parameter_Specification
            | A_Formal_Object_Declaration
            | An_Object_Renaming_Declaration
            =>
            Subtype_Indication := Simple_Name (Declaration_Subtype_Mark (Name_Decl));
         when A_Variable_Declaration
            | A_Constant_Declaration
            | A_Deferred_Constant_Declaration
            | A_Component_Declaration
            =>
            Subtype_Indication := Object_Declaration_View (Name_Decl);
            if Definition_Kind (Subtype_Indication) = A_Component_Definition then
               Subtype_Indication := Component_Definition_View (Subtype_Indication);
            end if;
            Subtype_Indication := Subtype_Simple_Name (Subtype_Indication);
         when others =>
            -- ?? Not appropriate (A_Single_Task_Declaration...)
            return Nil_Element_List;
      end case;

      Subtype_Indication := Type_Declaration_View (Corresponding_Name_Declaration (Subtype_Indication));
      case Type_Kind (Subtype_Indication) is
         when A_Record_Type_Definition
            | A_Derived_Record_Extension_Definition
            | A_Tagged_Record_Type_Definition
            =>
            return Record_Components (Asis.Definitions.Record_Definition (Subtype_Indication));
         when others =>
            -- not a record
            return  Nil_Element_List;
      end case;
   end Corresponding_Components;

   ------------------------------
   -- Ultimate_Expression_Type --
   ------------------------------

   function Ultimate_Expression_Type (The_Element : Asis.Expression) return Asis.Definition is
      use Asis.Definitions;
      Local_Elem : Asis.Element := A4G_Bugs.Corresponding_Expression_Type (The_Element);
   begin
      if Is_Nil (Local_Elem) then
         -- The_Element is a package, subprogram, task, anonymous access...
         -- For array, task and protected anonymous types, we can still go to the definition

         Local_Elem := Corresponding_Expression_Type_Definition (The_Element);
         if Is_Nil (Local_Elem) then
            -- There is definitely no type in sight here (it's a label f.e.)
            return Nil_Element;
         end if;

         case Declaration_Kind (Enclosing_Element (Local_Elem)) is
            when A_Type_Declaration | A_Subtype_Declaration =>
               -- Goddam! we were passed the name of a type
               return Type_Declaration_View (Ultimate_Type_Declaration (Enclosing_Element (Local_Elem)));
            when others =>
               return Local_Elem;
         end case;
      end if;

      -- Regular case: The element has a good ol' (named) type
      -- Go to the full declaration if necessary (incomplete and private)
      if Declaration_Kind (Local_Elem) in
        An_Incomplete_Type_Declaration .. A_Private_Extension_Declaration
      then
         Local_Elem := Corresponding_Full_Type_Declaration (Local_Elem);
      end if;

      Local_Elem := Type_Declaration_View (Corresponding_First_Subtype (Local_Elem));

      if Definition_Kind (Local_Elem) in
        A_Private_Type_Definition .. A_Private_Extension_Definition
      then
         Local_Elem := Type_Declaration_View (Corresponding_First_Subtype
                                                (Corresponding_Full_Type_Declaration
                                                   (Enclosing_Element (Local_Elem))));
      end if;

      if Type_Kind (Local_Elem) in
        A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
      then
         Local_Elem := Type_Declaration_View (Corresponding_Root_Type (Local_Elem));
      end if;

      return Local_Elem;
   end Ultimate_Expression_Type;


   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (The_Name : Asis.Expression) return Asis.Expression is
      use Asis.Expressions;
   begin
      if Expression_Kind (The_Name) = A_Selected_Component then
         return Selector (The_Name);
      else
         return The_Name;
      end if;
   end Simple_Name;

   --------------------
   -- Unindexed_Name --
   --------------------

   function Unindexed_Name (The_Name : Asis.Expression) return Asis.Expression is
      use Asis.Expressions;
   begin
      if Expression_Kind (The_Name) = An_Indexed_Component then
         return Selector (The_Name);
      else
         return The_Name;
      end if;
   end Unindexed_Name;

   -------------------
   -- Ultimate_Name --
   -------------------

   function Ultimate_Name (The_Name : Asis.Element; No_Component : Boolean := False) return Asis.Element is
      use Asis.Expressions;
      Decl   : Asis.Declaration;
      Result : Asis.Element := Simple_Name (The_Name);
   begin
      if Is_Nil (Result) then
         return Nil_Element;
      end if;

      if Expression_Kind (Result) = An_Attribute_Reference then
         -- would be nice to consider the Ultimate_Name of the prefix,
         -- but how do we rebuild the corresponding attribute?
         -- Leave as is for the moment
         return Result;
      end if;

      if Element_Kind (Result) = A_Defining_Name then
         Decl := Enclosing_Element (The_Name);
      else
         Decl := Corresponding_Name_Declaration (Result);
      end if;

      if Declaration_Kind (Decl) not in A_Renaming_Declaration then
         return Result;
      end if;

      -- There are cases (like renaming of an indexed component) where
      -- we want to unwind renamings, but Corrresponding_Base_Entity doesn't.
      -- Hence the loop.
   Going_Up_Renamings:
      while Declaration_Kind (Decl) in A_Renaming_Declaration loop
         Result := Corresponding_Base_Entity (Decl);
         loop
            case Expression_Kind (Result) is
               when A_Selected_Component =>
                  if Is_Access_Expression (Prefix (Result)) then
                     -- Implicit dereference
                     -- Normally dynamic (target unknown), except for the case of a pointer to tagged type used in
                     -- prefix notation for a subprogram call.
                     case Declaration_Kind (Corresponding_Name_Declaration (Selector (Result))) is
                        when A_Procedure_Declaration -- all callable entities
                           | A_Function_Declaration
                           | A_Procedure_Body_Declaration
                           | A_Function_Body_Declaration
                           | A_Null_Procedure_Declaration
                           | An_Expression_Function_Declaration
                           | A_Procedure_Renaming_Declaration
                           | A_Function_Renaming_Declaration
                           | An_Entry_Declaration
                           | A_Procedure_Instantiation
                           | A_Function_Instantiation
                           =>
                           Result := Selector (Result);
                        when others =>
                           Result := Nil_Element;
                           exit Going_Up_Renamings;
                     end case;
                  else
                     if No_Component
                       and then Declaration_Kind (Corresponding_Name_Declaration (Selector (Result)))
                     in A_Discriminant_Specification .. A_Component_Declaration
                     then
                        Result := Prefix (Result);
                     else
                        Result := Selector (Result);
                     end if;
                  end if;
               when A_Slice
                  | An_Indexed_Component
                    =>
                  if Is_Access_Expression (Prefix (Result)) then
                     -- Implicit dereference
                     Result := Nil_Element;
                     exit Going_Up_Renamings;
                  end if;

                  Result := Prefix (Result);
               when An_Explicit_Dereference
                  | A_Function_Call =>
                  Result := Nil_Element;
                  exit Going_Up_Renamings;
               when A_Type_Conversion =>
                  Result := Converted_Or_Qualified_Expression (Result);
               when An_Identifier
                  | An_Enumeration_Literal
                  | A_Character_Literal
                  | An_Operator_Symbol
                    =>
                  exit;
               when An_Attribute_Reference =>
                  -- Renaming of an attribute => return the attribute
                  exit Going_Up_Renamings;
               when others =>
                  Impossible ("Ultimate_Name: unexpected expression in renaming", Result);
            end case;
         end loop;
         Decl := Corresponding_Name_Declaration (Result);
      end loop Going_Up_Renamings;

      -- If A_Defining_Name, return A_Defining_Name
      if not Is_Nil (Result) and then Element_Kind (The_Name) = A_Defining_Name then
         Result := Corresponding_Name_Definition (Result);
      end if;

      return Result;
   end Ultimate_Name;


   -------------------------
   -- First_Defining_Name --
   -------------------------

   function First_Defining_Name (Name : Asis.Element) return Asis.Defining_Name is
      -- Note that this function is written to avoid calls to Corresponding_Body,
      -- since it is a big cause of tree swapping.
      use Asis.Expressions;
      Def        : Asis.Defining_Name;
      Decl       : Asis.Declaration;
      Other_Decl : Asis.Declaration;
   begin
      if Element_Kind (Name) = A_Defining_Name then
         Def := Name;
      else
         Def := Corresponding_Name_Definition (Simple_Name (Name));
         if Is_Nil (Def) then  -- predefined stuff without a declaration...
            return Nil_Element;
         end if;
      end if;

      -- Get declaration, but skip expanded defining names
      Decl := Enclosing_Element (Def);
      while Element_Kind (Decl) = A_Defining_Name loop
         Decl := Enclosing_Element (Decl);
      end loop;

      if Is_Subunit (Decl) then
         Decl := Corresponding_Body_Stub (Decl);
         Def  := Names (Decl)(1);
      end if;

      case Declaration_Kind (Decl) is
         when A_Constant_Declaration =>
            Other_Decl := Corresponding_Constant_Declaration (Def);
            if Is_Nil (Other_Decl) then
               return Def;
            else
               return Matching_Name (Def, Other_Decl);
            end if;
         when An_Ordinary_Type_Declaration
            | A_Task_Type_Declaration
            | A_Protected_Type_Declaration
              =>
            Other_Decl := Corresponding_Type_Partial_View (Decl);
            if Is_Nil (Other_Decl) then
               return Def;
            else
               return Matching_Name (Def, Other_Decl);
            end if;
         when A_Procedure_Declaration
            | A_Null_Procedure_Declaration   -- Ada 2005
            | A_Function_Declaration
            | An_Entry_Declaration
            | A_Package_Declaration
            | A_Single_Task_Declaration
            | A_Single_Protected_Declaration
              =>
            return Def;
         when A_Procedure_Body_Declaration
            | A_Function_Body_Declaration
            | An_Expression_Function_Declaration   -- Ada 2012
            | A_Procedure_Renaming_Declaration
            | A_Function_Renaming_Declaration
            | A_Procedure_Body_Stub
            | A_Function_Body_Stub
              =>
            Other_Decl := Corresponding_Declaration (Decl);
            if Is_Nil (Other_Decl) then
               return Def;
            else
               return Names (Other_Decl) (1);
            end if;
         when An_Entry_Body_Declaration
            | A_Package_Body_Declaration
            | A_Task_Body_Declaration
            | A_Protected_Body_Declaration
            | A_Package_Body_Stub
            | A_Task_Body_Stub
            | A_Protected_Body_Stub
              =>
            -- these have always a spec
            return Names (Corresponding_Declaration (Decl)) (1);

         when A_Parameter_Specification =>
            Other_Decl := Enclosing_Element (Decl);
            case Element_Kind (Other_Decl) is
               when A_Declaration =>
                  case Declaration_Kind (Other_Decl) is
                     when A_Procedure_Declaration
                        | A_Null_Procedure_Declaration   -- Ada 2005
                        | A_Function_Declaration
                        | An_Expression_Function_Declaration   -- Ada 2012
                        | A_Generic_Procedure_Declaration
                        | A_Generic_Function_Declaration
                        | An_Entry_Declaration
                        | A_Formal_Procedure_Declaration
                        | A_Formal_Function_Declaration
                          =>
                        return Def;
                     when A_Procedure_Body_Stub
                        | A_Function_Body_Stub
                          =>
                        Other_Decl := Corresponding_Declaration (Other_Decl);
                        if Is_Nil (Other_Decl) then
                           return Def;
                        else
                           return Matching_Formal_Name (Def, Other_Decl);
                        end if;
                     when A_Procedure_Body_Declaration
                        | A_Function_Body_Declaration
                        | A_Procedure_Renaming_Declaration
                        | A_Function_Renaming_Declaration
                        | An_Entry_Body_Declaration
                          =>
                        --Beware: these can be proper bodies
                        if Is_Subunit (Other_Decl) then
                           Other_Decl := Corresponding_Body_Stub (Other_Decl);
                           Def        := Matching_Formal_Name (Def, Other_Decl);
                        end if;
                        Other_Decl := Corresponding_Declaration (Other_Decl);
                        if Is_Nil (Other_Decl) then
                           return Def;
                        else
                           return Matching_Formal_Name (Def, Other_Decl);
                        end if;
                     when others =>
                        Impossible ("First_Defining_Name: not a callable entity (1)", Other_Decl);
                  end case;

               when A_Definition =>
                  case Definition_Kind (Other_Decl) is
                     when A_Type_Definition
                        | A_Formal_Type_Definition
                        | An_Access_Definition
                        =>
                        -- access to subprogram
                        return Def;
                     when others =>
                        Impossible ("First_Defining_Name: not a callable entity (2)", Other_Decl);
                  end case;

               when A_Statement =>
                  case Statement_Kind (Other_Decl) is
                     when An_Accept_Statement =>
                        return Matching_Formal_Name (Def, Corresponding_Entry (Other_Decl));
                     when others =>
                        Impossible ("First_Defining_Name: not an accept", Other_Decl);
                  end case;

               when others =>
                  Impossible ("First_Defining_Name: not a callable entity (3)", Other_Decl);
            end case;

         when others =>
            return Def;
      end case;
   end First_Defining_Name;

   -------------------
   -- Matching_Name --
   -------------------

   function Matching_Name (Name : Asis.Defining_Name; Decl : Asis.Declaration) return Asis.Defining_Name is
      Name_List : constant Asis.Defining_Name_List := Names (Decl);
      Name_Str  : constant Wide_String := To_Upper (Defining_Name_Image (Name));
   begin
      for N in Name_List'Range loop
         if To_Upper (Defining_Name_Image (Name_List (N))) = Name_Str then
            return Name_List (N);
         end if;
      end loop;
      -- not found
      return Nil_Element;
   end Matching_Name;


   -------------------------
   -- Ultimate_Expression --
   -------------------------

   function Ultimate_Expression (Expr : Asis.Expression) return Asis.Expression is
      use Asis.Expressions;
      Result  : Asis.Expression := Expr;
   begin
      loop
         case Expression_Kind (Result) is
            when An_Identifier =>
               if Declaration_Kind (Corresponding_Name_Declaration (Result)) /= A_Constant_Declaration then
                  exit;
               end if;
               Result := Initialization_Expression (Corresponding_Name_Declaration  (Result));
            when A_Selected_Component =>
               Result := Selector (Result);
            when others =>
               exit;
         end case;
      end loop;
      return Result;
   end Ultimate_Expression;

   -------------------------
   -- Association_Choices --
   -------------------------

   function Association_Choices (Assoc : Asis.Association) return Asis.Expression_List is
      use Asis.Expressions;
   begin
      case Association_Kind (Assoc) is
         when Not_An_Association =>
            Impossible ("Association_Choices: Not an association", Assoc);
         when A_Discriminant_Association =>
            return Discriminant_Selector_Names (Assoc);
         when A_Record_Component_Association =>
            return Record_Component_Choices (Assoc);
         when An_Array_Component_Association =>
            return Array_Component_Choices (Assoc);
         when A_Parameter_Association
            | A_Generic_Association
            | A_Pragma_Argument_Association
            =>
            declare
               Formal : constant Asis.Expression := Formal_Parameter (Assoc);
            begin
               if Is_Nil (Formal) then
                  return Nil_Element_List;
               else
                  return (1 => Formal);
               end if;
            end;
      end case;
   end Association_Choices;

   -----------------------
   -- Association_Value --
   -----------------------

   function Association_Value (Assoc : Asis.Association) return Asis.Expression is
      use Asis.Expressions;
   begin
      case Association_Kind (Assoc) is
         when Not_An_Association =>
            Impossible ("Association_Value: Not an association", Assoc);
         when A_Discriminant_Association =>
            return Discriminant_Expression (Assoc);
         when A_Record_Component_Association =>
            return Component_Expression (Assoc);
         when An_Array_Component_Association =>
            return Component_Expression (Assoc);

         when A_Parameter_Association
            | A_Generic_Association
            | A_Pragma_Argument_Association
            =>
            return Actual_Parameter (Assoc);
      end case;
   end Association_Value;


   -----------------------------------
   -- First_Enclosing_Instantiation --
   -----------------------------------

   function First_Enclosing_Instantiation (The_Element : Asis.Element) return Asis.Declaration is
      Result : Asis.Declaration := Enclosing_Element (The_Element);
   begin
      -- Note: we consider here that a formal package is an instantiation
      while Declaration_Kind (Result) not in A_Generic_Instantiation
            and Declaration_Kind (Result) not in A_Formal_Package_Declaration .. A_Formal_Package_Declaration_With_Box
      loop
         Result := Enclosing_Element (Result);
      end loop;

      return Result;
   end First_Enclosing_Instantiation;


   --------------------------------------
   -- Ultimate_Enclosing_Instantiation --
   --------------------------------------

   function Ultimate_Enclosing_Instantiation (The_Element : Asis.Element) return Asis.Declaration is
      Result : Asis.Declaration := Enclosing_Element (The_Element);
   begin
      while Is_Part_Of_Instance (Result) loop
         Result := Enclosing_Element (Result);
      end loop;

      return Result;
   end Ultimate_Enclosing_Instantiation;


   ---------------------
   -- Is_Generic_Unit --
   ---------------------

   function Is_Generic_Unit (Element : in Asis.Element) return Boolean is
   begin
      case Declaration_Kind (Element) is
         when A_Generic_Declaration =>
            return True;
         when A_Procedure_Body_Declaration
            | A_Function_Body_Declaration
            | A_Package_Body_Declaration
              =>
            if Is_Subunit (Element) then
                  return Is_Generic_Unit (Corresponding_Declaration (Corresponding_Body_Stub (Element)));
               else
                  return Is_Generic_Unit (Corresponding_Declaration (Element));
               end if;
         when others =>
            return False;
      end case;
   end Is_Generic_Unit;


   ------------------------
   -- Is_Part_Of_Generic --
   ------------------------

   function Is_Part_Of_Generic (Element : in Asis.Element) return Boolean is
      Parent_Name : constant Asis.Defining_Name := Enclosing_Program_Unit (Element);
      -- Parent_Name is the defining name of the enclosing program unit
      -- Its Enclosing_Element is the corresponding declaration
      Parent_Decl : Asis.Declaration;
   begin
      if Is_Nil (Parent_Name) then
         -- Element was the declaration of a compilation unit
         return False;
      end if;

      Parent_Decl := Enclosing_Element (Parent_Name);
      loop
         case Declaration_Kind (Parent_Decl) is
            when A_Generic_Declaration =>
               return True;
            when A_Procedure_Body_Declaration
               | A_Function_Body_Declaration
               | A_Package_Body_Declaration
                 =>
               if Is_Subunit (Parent_Decl) then
                  Parent_Decl := Corresponding_Declaration (Corresponding_Body_Stub (Parent_Decl));
               else
                  Parent_Decl := Corresponding_Declaration (Parent_Decl);
                  -- If there is no explicit specification, it cannot be generic
                  exit when Is_Nil (Parent_Decl);
               end if;
            when others =>
               exit;
         end case;
      end loop;

      return Is_Part_Of_Generic (Enclosing_Element (Parent_Name));
   end Is_Part_Of_Generic;


   ---------------------
   -- Ultimate_Origin --
   ---------------------

   function Ultimate_Origin (Element : in Asis.Element) return Asis.Unit_Origins is
      use Asis.Compilation_Units, Asis.Expressions;
      Decl : Asis.Declaration;
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            Decl := Element;
         when A_Defining_Name =>
            Decl := Enclosing_Element (Ultimate_Name (Element));
         when An_Expression =>
            Decl := Corresponding_Name_Declaration (Ultimate_Name (Element));
         when others =>
            Impossible ("Inappropriate element in Ultimate_Origin", Element);
      end case;

      if Is_Part_Of_Instance (Decl) then
         return Ultimate_Origin (Ultimate_Enclosing_Instantiation (Decl));
      end if;

      case Declaration_Kind (Decl) is
         when A_Generic_Instantiation =>
            return Ultimate_Origin (Generic_Unit_Name (Decl));
         when A_Renaming_Declaration =>
            return Ultimate_Origin (Renamed_Entity (Decl));
         when others =>
            return Unit_Origin (Enclosing_Compilation_Unit (Decl)); --## Rule line off Use_Ultimate_Origin
      end case;
   end Ultimate_Origin;


   ---------------------------------
   -- Definition_Compilation_Unit --
   ---------------------------------

   function Definition_Compilation_Unit (Element : in Asis.Element) return Asis.Compilation_Unit is
      use Asis.Expressions;
      Def : Asis.Defining_Name := Element;
   begin
      if Element_Kind (Element) = An_Expression then
         Def := Corresponding_Name_Definition (Simple_Name (Def));
      end if;

      return Enclosing_Compilation_Unit (Def);
   end Definition_Compilation_Unit;

   -----------------------
   -- Actual_Parameters --
   -----------------------

   function Actual_Parameters (Element : Asis.Element; Normalized : Boolean := False)
                              return Asis.Association_List
   is
      use Asis.Expressions;
   begin
      if Expression_Kind (Element) = A_Function_Call then
         return Function_Call_Parameters (Element, Normalized => Normalized);
      elsif Statement_Kind (Element) = A_Procedure_Call_Statement
        or  Statement_Kind (Element) = An_Entry_Call_Statement
      then
         return Call_Statement_Parameters (Element, Normalized => Normalized);
      elsif Declaration_Kind (Element) in A_Generic_Instantiation or
        Declaration_Kind (Element) = A_Formal_Package_Declaration
      then
         return Generic_Actual_Part (Element, Normalized => Normalized);
      else
         Impossible ("Unexpected element in Actual_Parameters", Element);
      end if;
   end Actual_Parameters;


   -----------------------------
   -- Constraining_Definition --
   -----------------------------

   function Constraining_Definition (E : Asis.Element) return Asis.Definition is
      use Asis.Definitions, Asis.Expressions;
      Current : Asis.Element := E;
   begin
      loop
         case Element_Kind (Current) is
            when A_Defining_Name =>
               Current := Enclosing_Element (Current);
            when A_Declaration =>
               case Declaration_Kind (Current) is
                  when A_Type_Declaration
                     | A_Subtype_Declaration
                     =>
                     Current := Type_Declaration_View (Current);
                  when A_Variable_Declaration
                     | A_Constant_Declaration
                     | A_Deferred_Constant_Declaration
                     | A_Component_Declaration
                     | A_Discriminant_Specification
                     | A_Parameter_Specification
                     | A_Single_Task_Declaration
                     | A_Single_Protected_Declaration
                     =>
                     Current := Object_Declaration_View (Current);
                  when A_Function_Declaration
                     | A_Function_Body_Declaration
                     | A_Function_Body_Stub
                     | A_Function_Renaming_Declaration
                     | A_Generic_Function_Declaration
                     | A_Formal_Function_Declaration
                     =>
                     Current := Result_Profile (Current);
                  when others =>
                     return Nil_Element;
               end case;
            when A_Definition =>
               case Definition_Kind (Current) is
                  when A_Subtype_Indication =>
                     if not Is_Nil (Subtype_Constraint (Current)) then
                        return Current;
                     end if;
                     Current := Subtype_Simple_Name (Current);
                  when A_Component_Definition =>
                     Current := Component_Definition_View (Current);
                  when A_Type_Definition
                     | A_Constraint
                     | A_Discrete_Range
                     =>
                     return Current;
                  when others =>
                     return Nil_Element;
               end case;
            when An_Expression =>
               case Expression_Kind (Current) is
                  when A_Qualified_Expression | A_Type_Conversion =>
                     Current := Converted_Or_Qualified_Subtype_Mark (Current);
                  when An_Attribute_Reference =>
                     return Nil_Element; --TBSL T'Base might or might not match T ...
                  when A_Selected_Component =>
                     Current := Selector (Current);
                  when An_Identifier =>
                     Current := Corresponding_Name_Declaration (Current);
                  when others =>
                     Current := A4G_Bugs.Corresponding_Expression_Type (Current);
               end case;
            when others =>
               return Nil_Element;
         end case;
      end loop;
   end Constraining_Definition;


   ----------------------------------
   -- Discrete_Constraining_Bounds --
   ----------------------------------

   function Discrete_Constraining_Bounds (Elem          : Asis.Element;
                                          Follow_Access : Boolean := False)
                                          return Asis.Element_List
   is
      use Asis.Definitions, Asis.Expressions;

      function Constraint_Bounds (Def : Asis.Constraint) return Expression_List;

      function Range_Attribute_Bounds (Attr : Asis.Expression) return Asis.Expression_List is
         Bounds   : constant Asis.Expression_List := Discrete_Constraining_Bounds (Prefix (Attr), Follow_Access);
         Dim_Expr : constant Asis.Expression_List := Attribute_Designator_Expressions (Attr);
         Dim      : List_Index;
      begin
         if Bounds'Length = 0 then
            return (1..2 => Nil_Element);
         end if;

         if Is_Nil (Dim_Expr) then
            Dim := 1;
         else
            -- In the extremely unlikely case where the static expression Dim_Expr is
            -- too complicated for us to evaluate, the following will raise Constraint_Error,
            -- and thus we will return "", which is appropriate.
            Dim := List_Index'Wide_Value (Static_Expression_Value_Image (Dim_Expr (1)));
         end if;

         -- Do not use a slice below, to ensure 'First = 1
         return (1=> Bounds (2*Dim-1), 2=> Bounds (2*Dim));
      end Range_Attribute_Bounds;

      function Discrete_Range_Bounds (Def : Asis.Discrete_Range) return Asis.Expression_List is
      begin
         case Discrete_Range_Kind (Def) is
            when Not_A_Discrete_Range =>
               Impossible ("Discrete_Range_Bounds: Not a discrete range", Def);
            when A_Discrete_Subtype_Indication =>
               if Is_Nil (Subtype_Constraint (Def)) then
                  return Discrete_Constraining_Bounds (Subtype_Simple_Name (Def), Follow_Access);
               else
                  return Constraint_Bounds (Subtype_Constraint (Def));
               end if;
            when A_Discrete_Range_Attribute_Reference =>
               return Range_Attribute_Bounds (Range_Attribute (Def));
            when A_Discrete_Simple_Expression_Range =>
               return (Lower_Bound (Def), Upper_Bound (Def));
         end case;
      end Discrete_Range_Bounds;

      function Constraint_Bounds (Def : Asis.Constraint) return Expression_List is
      begin
         case Constraint_Kind (Def) is
            when Not_A_Constraint =>
               Impossible ("Constraint_Bounds: Not a constraint", Def);
            when A_Digits_Constraint
              | A_Delta_Constraint
              | A_Discriminant_Constraint
              =>
               return Nil_Element_List; -- Not discrete
            when An_Index_Constraint =>
               declare
                  Index_Ranges : constant Asis.Definition_List := Discrete_Ranges (Def);
                  Result       : Asis.Expression_List (1 .. 2*Index_Ranges'Length);
               begin
                  for I in Index_Ranges'Range loop
                     declare
                        Bounds : constant Asis.Expression_List := Discrete_Range_Bounds (Index_Ranges (I));
                     begin
                        if Bounds = Nil_Element_List then
                           -- Non static...
                           Result (2 * I - 1 .. 2 * I) := (Nil_Element, Nil_Element);
                        else
                           Result (2 * I - 1 .. 2 * I) := Bounds;
                        end if;
                     end;
                  end loop;
                  return Result;
               end;
            when A_Simple_Expression_Range =>
               return (Lower_Bound (Def), Upper_Bound (Def));
            when A_Range_Attribute_Reference =>
               return Range_Attribute_Bounds (Range_Attribute (Def));
        end case;
      end Constraint_Bounds;

      function Choice_Bounds (E : Asis.Element) return Expression_List is
      begin
         if Element_Kind (E) = An_Expression then
            return (E, E);
         else
            -- Must be a range
            return Discrete_Range_Bounds (E);
         end if;
      end Choice_Bounds;

      Item             : Asis.Element := Elem; -- This item will navigate until we find the appropriate definition
      Old_Item         : Asis.Element;
      Constraint       : Asis.Definition;
      No_Unconstrained : Boolean := False;
      -- In the case of an unconstrained array type, we normally return the indices of the index type.
      -- However, in the case of an object of an unconstrained type (including the returned object of
      -- a function call), the constraint is inherited from the initial (or actual) value, and hence not
      -- available. No_Unconstrained is set to True when moving from an object declaration (or function
      -- call) to its type to prevent returning the indices of the unconstrained type.
   begin  -- Discrete_Constraining_Bounds
      loop
         case Element_Kind (Item) is
            when A_Definition =>                 ----------------- Definitions
               case Definition_Kind (Item) is
                  when Not_A_Definition =>
                     Impossible ("Discrete_Constraining_Bounds: not a definition", Item);

                  when A_Constraint =>
                     return Constraint_Bounds (Item);

                  when A_Discrete_Range
                     | A_Discrete_Subtype_Definition
                       =>
                     return Discrete_Range_Bounds (Item);

                  when A_Subtype_Indication =>
                     Constraint := Subtype_Constraint (Item);
                     if Is_Nil (Constraint) then
                        Item := Subtype_Simple_Name (Item);
                     else
                        Item := Constraint;
                     end if;

                  when A_Type_Definition =>
                     case Type_Kind (Item) is
                        when Not_A_Type_Definition =>
                           Impossible ("Discrete_Constraining_Bounds: not a type definition", Item);
                        when A_Derived_Type_Definition =>
                           Item := Parent_Subtype_Indication (Item);
                        when A_Derived_Record_Extension_Definition
                           | A_Record_Type_Definition
                           | A_Tagged_Record_Type_Definition
                             =>
                           -- Not a discrete type
                           return Nil_Element_List;
                        when An_Access_Type_Definition =>
                           if not Follow_Access then
                              return Nil_Element_List;
                           end if;
                           Item := Asis.Definitions.Access_To_Object_Definition (Item);
                        when An_Enumeration_Type_Definition =>
                           declare
                              Literals : constant Asis.Declaration_List := Enumeration_Literal_Declarations (Item);
                           begin
                              return (Names (Literals (Literals'First)) (1), Names (Literals (Literals'Last)) (1));
                           end;
                        when A_Modular_Type_Definition =>
                           return (Nil_Element, Mod_Static_Expression (Item));
                        when A_Signed_Integer_Type_Definition =>
                           Item := Integer_Constraint (Item);
                        when A_Root_Type_Definition =>
                           case Root_Type_Kind (Item) is
                              when A_Root_Integer_Definition
                                 | A_Universal_Integer_Definition
                                   =>
                                 -- Bounds of Root_Integer are System.Min_Int .. System.Max_Int, 3.5.4(14)
                                 -- However, we have no (easy) way to retrieve them as Element
                                 -- Bounds of Universal_Integer are infinite.
                                 -- Just pretend they are both not computable
                                 return (Nil_Element, Nil_Element);
                              when A_Root_Real_Definition
                                 | A_Universal_Real_Definition
                                 | A_Universal_Fixed_Definition
                                   =>
                                 -- No way to get the bounds...
                                 return (Nil_Element, Nil_Element);
                              when Not_A_Root_Type_Definition =>
                                 Impossible ("Discrete_Constraining_Bounds: Wrong root type", Item);
                           end case;
                        when A_Floating_Point_Definition
                           | An_Ordinary_Fixed_Point_Definition
                           | A_Decimal_Fixed_Point_Definition
                             =>
                           Item := Real_Range_Constraint (Item);
                           if Is_Nil (Item) then
                              return (Nil_Element, Nil_Element);
                           end if;
                        when An_Unconstrained_Array_Definition =>
                           declare
                              Index_Defs : constant Asis.Definition_List := Index_Subtype_Definitions (Item);
                              Result     : Asis.Expression_List (1 .. 2 * Index_Defs'Length);
                           begin
                              if No_Unconstrained then
                                 Result := (others => Nil_Element);
                              else
                                 -- Index_Defs can only contain A_Discrete_Subtype_Indication here
                                 for I in Index_Defs'Range loop
                                    Result (2 * I - 1 .. 2 * I) := Discrete_Constraining_Bounds (Index_Defs (I),
                                                                                                 Follow_Access);
                                 end loop;
                              end if;

                              return Result;
                           end;
                        when A_Constrained_Array_Definition =>
                           declare
                              Index_Defs : constant Asis.Definition_List := Discrete_Subtype_Definitions (Item);
                              Result     : Asis.Expression_List (1 .. 2 * Index_Defs'Length);
                           begin
                              for I in Index_Defs'Range loop
                                 Result (2 * I - 1 .. 2 * I) := Discrete_Range_Bounds (Index_Defs (I));
                              end loop;

                              return Result;
                           end;
                        when others =>  -- Compatibility Ada 2005
                           return Nil_Element_List;
                     end case;

                  when A_Component_Definition =>
                     Item := Component_Definition_View (Item);

                  when A_Formal_Type_Definition =>
                     case Formal_Type_Kind (Item) is
                        when Not_A_Formal_Type_Definition =>
                           Impossible ("Discrete_Constraining_Bounds: not a formal definition", Item);
                        when A_Formal_Private_Type_Definition
                           | A_Formal_Tagged_Private_Type_Definition
                             =>
                           return Nil_Element_List;
                        when A_Formal_Derived_Type_Definition =>
                           Item := Subtype_Simple_Name (Item);
                        when A_Formal_Discrete_Type_Definition
                           | A_Formal_Signed_Integer_Type_Definition
                           | A_Formal_Modular_Type_Definition
                             =>
                           return (Nil_Element, Nil_Element);
                        when A_Formal_Floating_Point_Definition
                           | A_Formal_Ordinary_Fixed_Point_Definition
                           | A_Formal_Decimal_Fixed_Point_Definition
                             =>
                           return Nil_Element_List;
                        when A_Formal_Access_Type_Definition =>
                           if not Follow_Access then
                              return Nil_Element_List;
                           end if;
                           Item := Asis.Definitions.Access_To_Object_Definition (Item);
                        when A_Formal_Unconstrained_Array_Definition =>
                           declare
                              Index_Defs : constant Asis.Definition_List := Index_Subtype_Definitions (Item);
                              Result     : Asis.Expression_List (1 .. 2 * Index_Defs'Length);
                           begin
                              if No_Unconstrained then
                                 Result := (others => Nil_Element);
                              else
                                 -- Index_Defs can only contain A_Discrete_Subtype_Indication here
                                 for I in Index_Defs'Range loop
                                    Result (2 * I - 1 .. 2 * I) := Discrete_Constraining_Bounds (Index_Defs (I),
                                                                                                 Follow_Access);
                                 end loop;
                              end if;

                              return Result;
                           end;
                        when A_Formal_Constrained_Array_Definition =>
                           declare
                              Index_Defs : constant Asis.Definition_List := Discrete_Subtype_Definitions (Item);
                              Result     : Asis.Expression_List (1 .. 2 * Index_Defs'Length);
                           begin
                              for I in Index_Defs'Range loop
                                 Result (2 * I - 1 .. 2 * I) := Discrete_Range_Bounds (Index_Defs (I));
                              end loop;

                              return Result;
                           end;
                        when others =>  -- Compatibility Ada 2005
                           return Nil_Element_List;
                     end case;

                  when A_Task_Definition
                     | A_Protected_Definition
                     | An_Unknown_Discriminant_Part
                     | A_Known_Discriminant_Part
                     | A_Record_Definition
                     | A_Null_Record_Definition
                     | A_Null_Component
                     | A_Variant_Part
                     | A_Variant
                     | An_Others_Choice
                     | A_Private_Type_Definition
                     | A_Tagged_Private_Type_Definition
                     | A_Private_Extension_Definition
                       =>
                     -- Not a discrete type
                     return Nil_Element_List;

                  when others =>  -- Compatibility Ada 2005
                     return Nil_Element_List;
               end case;                         ----------------- Definitions

            when An_Expression =>                ----------------- Expressions
               case Expression_Kind (Item) is
                  when A_Selected_Component =>
                     Item := Selector (Item);
                  when An_Indexed_Component =>
                     -- We must go to the declaration of the component type of the array
                     Item := A4G_Bugs.Corresponding_Expression_Type (Item);
                  when A_Slice =>
                     Item := Slice_Range (Item);
                  when An_Explicit_Dereference =>
                     -- We must go to the declaration of the type referenced by the prefix
                     Item := Access_Target_Type (Corresponding_Expression_Type_Definition (Prefix (Item)));
                     if Is_Nil (Item) then
                        -- Access to SP...
                        return Nil_Element_List;
                     end if;
                  when An_Attribute_Reference =>
                     -- We cannot get to the bounds of T'Base, and taking T instead would be misleading
                     -- There is no applicable constraints for 'Class
                     -- Other attributes are values or subprograms, not interesting for us.
                     -- Therefore, in all cases:
                     return Nil_Element_List;
                  when A_Function_Call =>
                     -- The constraint is the one of the return type
                     No_Unconstrained := True;
                     Item             := A4G_Bugs.Corresponding_Expression_Type (Item);
                  when A_Named_Array_Aggregate
                     | A_Positional_Array_Aggregate
                     | A_String_Literal
                     =>
                     declare
                        Item_Def : Asis.Definition := Corresponding_Expression_Type_Definition (Item);
                     begin
                        if Is_Nil (Item_Def) then
                           -- Type cannot be determined, may be that this aggregate is assigned to
                           -- a variable of an anonymous array type.
                           -- TBSL Are there other contexts where an aggregate of an anonymous type can be used?
                           if Statement_Kind (Enclosing_Element (Item)) /= An_Assignment_Statement then
                              -- in despair...
                              return Nil_Element_List;
                           end if;

                           -- Assignment: the bounds are the same as the LHS
                           --  TBSL: is that really true (sliding)?
                           Item_Def := Corresponding_Expression_Type_Definition
                                          (Assignment_Variable_Name
                                           (Enclosing_Element (Item)));
                        end if;

                        if Expression_Kind (Item) = A_Named_Array_Aggregate then
                           -- Named aggregates may (sometimes) have their bounds defined
                           -- from the aggregate itself ...
                           -- TBSL for the moment, we deal only with the first dimension, and consider
                           -- other dimensions to be dynamic. This can of course be improved, but 4.3.3
                           -- is such a nightmare...
                           declare
                              Assocs         : constant Association_List := Array_Component_Associations (Item);
                              Current_Bounds : Expression_List (1 .. 2);
                              Extreme_Bounds : Expression_List (1 .. 2);
                              Extreme_Vals   : Extended_Biggest_Int_List (1 .. 2)
                                               := (Biggest_Int'Last, Biggest_Int'First);
                              Value          : Extended_Biggest_Int;
                              Encl           : Asis.Element;
                           begin
                              -- ... not if there is an others choice
                              if Definition_Kind (Array_Component_Choices (Assocs (Assocs'Last)) (1))
                                = An_Others_Choice
                              then
                                 -- The bounds are given by the applicable index constraint.
                                 -- TBSL for the moment, we deal only with simple cases
                                 loop
                                    Encl := Enclosing_Element (Item);
                                    exit when Expression_Kind (Encl) /= A_Parenthesized_Expression;
                                 end loop;
                                 if Statement_Kind (Encl) = An_Assignment_Statement then
                                    -- Assignment: the bounds are the same as the LHS
                                    return Discrete_Constraining_Bounds (Assignment_Variable_Name (Encl));
                                 elsif Expression_Kind (Encl) = A_Qualified_Expression then
                                    return Discrete_Constraining_Bounds
                                            (Converted_Or_Qualified_Subtype_Mark (Encl));
                                 elsif Declaration_Kind (Encl) in A_Variable_Declaration .. A_Constant_Declaration then
                                    return Discrete_Constraining_Bounds (Encl);
                                 end if;
                              else
                                 if Assocs'Length = 1 and then Array_Component_Choices (Assocs (1))'Length = 1 then
                                    -- Only case where the bounds can be dynamic
                                    return Choice_Bounds (Array_Component_Choices (Assocs (1)) (1))
                                      & (3 .. 2 * Dimensionality (Item_Def) => Nil_Element);
                                 else
                                    -- All bounds (and ranges) static: find the extremes
                                    for A in Assocs'Range loop
                                       declare
                                          Choices_List : constant Asis.Expression_List
                                            :=  Array_Component_Choices (Assocs (A));
                                       begin
                                          for C in Choices_List'Range loop
                                             Current_Bounds :=  Choice_Bounds (Choices_List (C));
                                             -- Low
                                             Value := Discrete_Static_Expression_Value (Current_Bounds (1));
                                             if Value < Extreme_Vals (1) then
                                                Extreme_Vals   (1) := Value;
                                                Extreme_Bounds (1) := Current_Bounds (1);
                                             end if;
                                             --High
                                             Value := Discrete_Static_Expression_Value (Current_Bounds (2));
                                             if Value > Extreme_Vals (2) then
                                                Extreme_Vals   (2) := Value;
                                                Extreme_Bounds (2) := Current_Bounds (2);
                                             end if;
                                          end loop;
                                       end;
                                    end loop;
                                    return Extreme_Bounds
                                      & (3 .. 2 * Dimensionality (Item_Def) => Nil_Element);
                                 end if;
                              end if;
                           end;
                        end if;

                        Item := Item_Def;
                     end;

                  when A_Qualified_Expression
                     | A_Type_Conversion
                     =>
                     -- Easy: use the given type!
                     Item := Converted_Or_Qualified_Subtype_Mark (Item);

                  when A_Parenthesized_Expression =>
                     Item := Expression_Parenthesized (Item);

                  when An_If_Expression
                     | A_Case_Expression
                     =>
                     -- These are always dynamic, since different paths may have different constraints
                     -- Get the dimensionality from the first path, and replace all bounds with Nil_Element
                     declare
                        Path1_Bounds : Asis.Element_List := Discrete_Constraining_Bounds
                                                             (Dependent_Expression (Expression_Paths (Item) (1)),
                                                              Follow_Access);
                     begin
                        for P in Path1_Bounds'Range loop
                           Path1_Bounds (P) := Nil_Element;
                        end loop;
                        return Path1_Bounds;
                     end;

                  when others =>
                     -- Assume it's a name, but it can be a type name, therefore
                     -- we cannot take directly Corresponding_Expression_Type
                     Item := Corresponding_Name_Declaration (Item);
               end case;                         ----------------- Expressions

            when A_Defining_Name =>              ----------------- Defining name
               Old_Item := Item;
               Item     := Enclosing_Element (Item);
               if Declaration_Kind (Item) = A_Deferred_Constant_Declaration then
                  -- See below in path for A_Deferred_Constant_Declaration why it is special-cased here
                  Item := Corresponding_Constant_Declaration (Old_Item);
               end if;

            when A_Declaration =>                ----------------- Declarations
               case Declaration_Kind (Item) is
                  when An_Ordinary_Type_Declaration
                     | A_Subtype_Declaration
                     | A_Formal_Type_Declaration
                       =>
                     Item := Type_Declaration_View (Item);

                  when A_Task_Type_Declaration
                     | A_Protected_Type_Declaration
                     | A_Private_Extension_Declaration
                       =>
                     return Nil_Element_List;

                  when An_Incomplete_Type_Declaration
                     | A_Private_Type_Declaration
                       =>
                     Item := Corresponding_Full_Type_Declaration (Item);

                  when A_Variable_Declaration
                     | A_Constant_Declaration
                     | A_Component_Declaration
                     | A_Return_Constant_Specification
                     | A_Return_Variable_Specification
                       =>
                     No_Unconstrained := True;
                     Item             := Object_Declaration_View (Item);

                  when A_Deferred_Constant_Declaration =>
                     Item := Corresponding_Constant_Declaration (Names (Item) (1));
                     -- Note that in the case of multiple declarations, different identifiers might have
                     -- different bounds, e.g.:
                     --    A, B : constant String;
                     -- private
                     --   A : constant String := "abc";
                     --   B : constant String := "abcdef";
                     -- But we can come here only if directly given the declaration; if we were initially
                     -- coming from a defining name, it is special cased there.
                     -- Therefore returning the constraint of the first element makes as much sense as
                     -- anything else...

                  when A_Loop_Parameter_Specification =>
                     No_Unconstrained := True;
                     Item             := Specification_Subtype_Definition (Item);

                  when A_Parameter_Specification
                     | A_Formal_Object_Declaration
                       =>
                     No_Unconstrained := True;
                     Item             := Object_Declaration_View (Item);

                  when An_Object_Renaming_Declaration =>
                     Item := Renamed_Entity (Item);

                  when An_Element_Iterator_Specification =>
                     Item := Iteration_Scheme_Name (Item);

                  when others =>
                     Impossible ("Discrete_Constraining_Bounds: Bad declaration "
                                   & Asis.Declaration_Kinds'Wide_Image (Declaration_Kind (Item)),
                                 Item);
               end case;

            when others =>
               Impossible ("Discrete_Constraining_Bounds: Inappropriate element", Item);
         end case;                               ----------------- Declarations
      end loop;
   end Discrete_Constraining_Bounds;


   -----------------------------------
   -- Discrete_Constraining_Values --
   -----------------------------------

   function Discrete_Constraining_Values (Elem          : Asis.Element;
                                          Follow_Access : Boolean := False)
                                          return Extended_Biggest_Int_List
   is
      Bounds : constant Asis.Element_List := Discrete_Constraining_Bounds (Elem, Follow_Access);
      Result : Extended_Biggest_Int_List (Bounds'Range);
      Modular_Type : Boolean := False;
   begin
      if Result'Length = 0 then
         return Nil_Extended_Biggest_Int_List;
      end if;

      for I in Result'Range loop
         begin
            case Element_Kind (Bounds (I)) is
               when An_Expression =>
                  Result (I) := Discrete_Static_Expression_Value (Bounds (I));
                  if Modular_Type then
                     -- The value returned for the upper bound by discrete_constraining_bounds is the mod expression
                     -- => 1 more than the upper bound
                     Result (I) := Result (I) - 1;
                  end if;
                  Modular_Type := False;
               when A_Defining_Name =>
                  -- Enumeration
                  Result (I) := Biggest_Int'Wide_Value (Position_Number_Image (Bounds (I)));
                  Modular_Type := False;
               when Not_An_Element =>
                  if I rem 2 = 1 and then not Is_Nil (Bounds (I + 1)) then
                     -- Lower bound of modular type (not subtype)
                     Result (I)   := 0;
                     Modular_Type := True;
                  else
                     Result (I)   := Not_Static;
                     Modular_Type := False;
                  end if;
               when others =>
                  Impossible ("Bad return from Discrete_Range_Bounds", Bounds (2));
            end case;
         exception
            when Constraint_Error =>
               -- Not in range of Biggest_Int...
               Result (I) := Not_Static;
               Modular_Type := False;
         end;
      end loop;

      return Result;
   end Discrete_Constraining_Values;


   -----------------------------------
   -- Discrete_Constraining_Lengths --
   -----------------------------------

   function Discrete_Constraining_Lengths (Elem          : Asis.Element;
                                           Follow_Access : Boolean := False)
                                           return Extended_Biggest_Natural_List
   is
      Bounds : constant Extended_Biggest_Int_List := Discrete_Constraining_Values (Elem, Follow_Access);
      Result : Extended_Biggest_Natural_List (1 .. Bounds'Length / 2);
   begin
      if Result'Length = 0 then
         return Nil_Extended_Biggest_Natural_List;
      end if;

      for I in Result'Range loop
         declare
            Low  : constant Extended_Biggest_Int := Bounds (2 * I - 1);
            High : constant Extended_Biggest_Int := Bounds (2 * I);
         begin
            if Low = Not_Static or High = Not_Static then
               -- Some bound is dynamic
               Result (I) := Not_Static;
            elsif Low > High then
               Result (I) := 0;
            else
               Result (I) := High - Low + 1;
            end if;
         exception
            when Constraint_Error =>
               -- Not in range of Biggest_Int...
               Result (I) := Not_Static;
         end;
      end loop;

      return Result;
   end Discrete_Constraining_Lengths;

   ---------------------------
   -- Are_Matching_Subtypes --
   ---------------------------

   function Are_Matching_Subtypes (Left, Right : Asis.Element) return Boolean is
      Left_Def  : constant Asis.Definition := Constraining_Definition (Left);
      Right_Def : constant Asis.Definition := Constraining_Definition (Right);
   begin   -- Are_Matching_Subtypes
      if Is_Nil (Left_Def) or Is_Nil (Right_Def) then
         return False;
      end if;

      if Is_Equal (Left_Def, Right_Def) then
         return True;
      end if;

      if Discrete_Constraining_Values (Left_Def) = Discrete_Constraining_Values (Right_Def) then
         return True;
      end if;

      return False;
   end Are_Matching_Subtypes;

   ----------------
   -- Statements --
   ----------------

   function Statements (Element         : in Asis.Element;       --## rule line off LOCAL_HIDING
                        Include_Pragmas : in Boolean := False)
                        return Asis.Statement_List
   is
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Body_Declaration
                  | A_Procedure_Body_Declaration
                  | An_Entry_Body_Declaration
                  | A_Package_Body_Declaration
                  | A_Task_Body_Declaration
                 =>
                  return Body_Statements (Element, Include_Pragmas);
               when others =>
                  Impossible ("Statements: invalid declaration kind", Element);
            end case;
         when A_Statement =>
            case Statement_Kind (Element) is
               when An_Accept_Statement =>
                  return Accept_Body_Statements (Element, Include_Pragmas);
               when A_Block_Statement =>
                  return Block_Statements (Element, Include_Pragmas);
               when A_Loop_Statement
                  | A_While_Loop_Statement
                  | A_For_Loop_Statement
                    =>
                  return Loop_Statements (Element, Include_Pragmas);
               when An_Extended_Return_Statement =>
                  return Extended_Return_Statements (Element, Include_Pragmas);
               when others =>
                  Impossible ("Statements: invalid statement kind", Element);
            end case;
         when A_Path =>
            return Sequence_Of_Statements (Element, Include_Pragmas);
         when An_Exception_Handler =>
            return Handler_Statements (Element, Include_Pragmas);
         when others =>
            Impossible ("Statements: invalid element kind", Element);
      end case;
   end Statements;

   ------------------------------
   -- Last_Effective_Statement --
   ------------------------------

   function Last_Effective_Statement (Stats : Asis.Statement_List) return Asis.Statement is
      Result : Asis.Statement := Stats (Stats'Last);
   begin
      while Statement_Kind (Result) = A_Block_Statement
        and then Is_Nil (Block_Exception_Handlers (Result))
      loop
         declare
            Block_Stmts : constant Asis.Statement_List := Block_Statements (Result);
         begin
            Result := Block_Stmts (Block_Stmts'Last);
         end;
      end loop;
      return Result;
   end Last_Effective_Statement;

   -------------------------
   -- Are_Null_Statements --
   -------------------------

   function Are_Null_Statements (Stats : Asis.Statement_List; Except_Labelled : Boolean := False) return Boolean is
   begin
      for I in Stats'Range loop
         case Statement_Kind (Stats (I)) is
            when A_Null_Statement =>
               null;
            when A_Block_Statement =>
               if Block_Declarative_Items (Stats (I)) /= Nil_Element_List
                 or else not Are_Null_Statements (Block_Statements (Stats (I)))
               then
                  return False;
               end if;
            when others =>
               return False;
         end case;

         if Except_Labelled and then not Is_Nil (Label_Names (Stats (I)))then
            return False;
         end if;
      end loop;
      return True;
   end Are_Null_Statements;


   -----------------------------
   -- First_Exiting_Statement --
   -----------------------------

   function First_Exiting_Statement (Stats : Asis.Statement_List; Include_Returns : Boolean := True)
                                     return Asis.Statement
   is
      use Asis.Iterator;

      procedure Pre_Operation  (Element :        Asis.Element;
                                Control : in out Traverse_Control;
                                State   : in out Asis.Statement);
      procedure Post_Operation (Element :        Asis.Element;
                                Control : in out Traverse_Control;
                                State   : in out Asis.Statement) is null;
      procedure Traverse is new Traverse_Element (Asis.Statement, Pre_Operation, Post_Operation);

      procedure Pre_Operation (Element :        Asis.Element;
                               Control : in out Traverse_Control;
                               State   : in out Asis.Statement)
      is
      begin
         if Element_Kind (Element) /= A_Statement then
            return;
         end if;

         case Statement_Kind (Element) is
            when An_Exit_Statement =>
               if not Is_Part_Of (Corresponding_Loop_Exited (Element), Stats) then
                  State   := Element;
                  Control := Terminate_Immediately;
               end if;
            when A_Goto_Statement =>
               if not Is_Part_Of (Corresponding_Destination_Statement (Element), Stats) then
                  State   := Element;
                  Control := Terminate_Immediately;
               end if;
            when A_Return_Statement
               | An_Extended_Return_Statement
               | A_Requeue_Statement
               | A_Requeue_Statement_With_Abort
               =>
               if Include_Returns then
                  State   := Element;
                  Control := Terminate_Immediately;
               end if;
            when others =>
               null;
         end case;
      end Pre_Operation;

      Exiting_Stmt : Asis.Statement   := Nil_Element;
      Control      : Traverse_Control := Continue;
   begin    -- First_Exiting_Statement
      for S in Stats'Range loop
         Traverse (Stats (S), Control, Exiting_Stmt);
         exit when not Is_Nil (Exiting_Stmt);
      end loop;
      return Exiting_Stmt;
   end First_Exiting_Statement;


   ----------------
   -- Is_Part_Of --
   ----------------

   function Is_Part_Of (Elem : Asis.Element; Inside : Asis.Element_List) return Boolean is
      use Asis.Compilation_Units, Asis.Text;

      Elem_Span  : Span;
      Start_Span : Span;
      Stop_Span  : Span;
   begin
      if Inside = Nil_Element_List then
         return False;
      end if;

      if not Is_Equal (Enclosing_Compilation_Unit (Elem),
                       Enclosing_Compilation_Unit (Inside (Inside'First)))
      then
         return  False;
      end if;

      Elem_Span  := Element_Span (Elem);
      Start_Span := Element_Span (Inside (Inside'First));
      Stop_Span  := Element_Span (Inside (Inside'Last));
      if Elem_Span.First_Line not in Start_Span.First_Line .. Stop_Span.Last_Line then
         return False;
      end if;
      if Elem_Span.First_Line = Start_Span.First_Line and Elem_Span.First_Column < Start_Span.First_Column then
         return False;
      end if;
      if Elem_Span.Last_Line = Stop_Span.Last_Line and Elem_Span.Last_Column > Stop_Span.Last_Column then
         return False;
      end if;
      return True;
   end Is_Part_Of;


   ------------------------
   -- Exception_Handlers --
   ------------------------

   function Exception_Handlers (Element : Asis.Element) return Asis.Exception_Handler_List is
   begin
      case Element_Kind (Element) is
         when A_Declaration =>
            case Declaration_Kind (Element) is
               when A_Function_Body_Declaration
                 | A_Procedure_Body_Declaration
                 | An_Entry_Body_Declaration
                 | A_Package_Body_Declaration
                 | A_Task_Body_Declaration
                 =>
                  return Body_Exception_Handlers (Element);
               when others =>
                  Impossible ("Exception_Handlers: invalid declaration kind", Element);
            end case;
         when A_Statement =>
            case Statement_Kind (Element) is
               when An_Accept_Statement =>
                  return Accept_Body_Exception_Handlers (Element);
               when A_Block_Statement =>
                  return Block_Exception_Handlers (Element);
               when An_Extended_Return_Statement =>
                  return Extended_Return_Exception_Handlers (Element);
               when others =>
                  Impossible ("Exception_Handlers: invalid statement kind", Element);
            end case;
         when others =>
            Impossible ("Exception_Handlers: invalid element kind", Element);
      end case;
   end Exception_Handlers;

   ----------------------
   -- Size_Value_Image --
   ----------------------

   ----------------------
   -- Size_Value_Image --
   ----------------------

   function Size_Value_Image (Name : Asis.Element) return Wide_String is
      use Asis.Clauses, Asis.Definitions, Asis.Expressions;

      Expr      : Asis.Expression;
      Def       : Asis.Definition;
      Decl      : Asis.Declaration;
      Good_Name : Asis.Expression := Simple_Name (Name);
   begin
      case Element_Kind (Good_Name) is
         when An_Expression =>
            case Expression_Kind (Good_Name) is
               when An_Indexed_Component =>
                  -- Special case: cannot be a type, only component_size is relevant
                  Decl := A4G_Bugs.Corresponding_Expression_Type (Prefix (Good_Name));
                  if Is_Nil (Decl) then
                     -- Anonymous array, cannot specify Component_Size
                     return "";
                  end if;
                  Expr := Attribute_Clause_Expression (A_Component_Size_Attribute, Decl);
                  if Is_Nil (Expr) then
                     return "";
                  end if;
                  return Static_Expression_Value_Image (Expr);

               when A_Slice =>
                  -- Yes, taking Tab(1..5)'Size is allowed.
                  -- TBSL: Requires Discrete_Constraining_Lengths to be able to process a slice
                  -- Give up for the moment
                  return "";

               when An_Attribute_Reference =>
                  case Attribute_Kind (Good_Name) is
                     when A_Base_Attribute =>
                        -- The prefix has to be scalar
                        -- The base type is at least the first named subtype => go there
                        Decl := Corresponding_First_Subtype
                                (Corresponding_Name_Declaration
                                 (Simple_Name (Prefix (Good_Name))));
                        case Scalar_Types (Type_Category (Decl)) is
                           when An_Enumeration_Type =>
                              -- Base type is the same as the type
                              Good_Name := Names (Decl)(1);
                           when Integer_Types =>
                              -- Get the size for the type, and extend it to the nearest power of 2
                              -- Not guaranteed by the language, but very likely in practice
                              Good_Name := Names (Decl) (1);
                              case Size_Value (Good_Name) is
                                 when Not_Static =>
                                    return "";
                                 when 0 .. 8 =>
                                    return "8";
                                 when 9 .. 16 =>
                                    return "16";
                                 when 17 .. 32 =>
                                    return "32";
                                 when 33 .. 64 =>
                                    return "64";
                                 when 65 .. 128 =>
                                    return "128";
                                 when others =>
                                    Impossible ("Size of integer type > 128", Good_Name);
                              end case;
                           when Real_Types =>
                              -- Hard to tell what the compiler does...
                              -- Wait for ASIS 2012
                              return "";
                        end case;
                     when A_Class_Attribute =>
                        -- Always indefinite => implementation defined (13.3(48))
                        -- Give up
                        return "";
                     when others =>
                          Impossible ("Size_Value_Image: Attribute not 'Base or 'Class", Good_Name);
                  end case;

               when An_Explicit_Dereference =>
                  -- Size of dynamically allocated object cannot be specified
                  --   => No hope to find out
                  return "";

               when An_Identifier =>
                  Good_Name := Ultimate_Name (Good_Name);
                  Def       := Corresponding_Name_Definition (Good_Name);
                  Decl      := Corresponding_Name_Declaration (Good_Name);

               when others =>
                  Impossible ("Size_Value_Image: wrong expression", Name);
            end case;

         when A_Defining_Name =>
            Decl := Enclosing_Element (Good_Name);
            Def  := Good_Name;

         when others =>
            Impossible ("Size_Value_Image: wrong element", Name);
      end case;

      -- Do we have a size clause (type or object)?
      Expr := Attribute_Clause_Expression (A_Size_Attribute, Good_Name);
      if not Is_Nil (Expr) then
         -- we have a size clauseie
         return Static_Expression_Value_Image (Expr);
      end if;

      case Declaration_Kind (Decl) is
         when An_Ordinary_Type_Declaration =>
            -- Is it a type derived from a type with a computable size?
            if Type_Kind (Type_Declaration_View (Decl)) = A_Derived_Type_Definition then
               return Size_Value_Image (Subtype_Simple_Name (Parent_Subtype_Indication (Type_Declaration_View (Decl))));
            end if;

            -- Last chance: some predefined stuff whose size is known (but not necessarily represented
            -- properly with a size clause)
            declare
               Pfx : constant Wide_String := To_Upper (Full_Name_Image (Good_Name));
            begin
               if Pfx = "STANDARD.BOOLEAN" then
                  return "1";
               elsif Pfx = "STANDARD.CHARACTER" then
                  return "8";
               elsif Pfx = "STANDARD.INTEGER" then
                  return Biggest_Int_Img (Integer'Size);
               elsif Pfx = "STANDARD.FLOAT" then
                  return Biggest_Int_Img (Float'Size);

               elsif Pfx = "STANDARD.LONG_INTEGER" then
                  return Biggest_Int_Img (Long_Integer'Size);
               elsif Pfx = "STANDARD.LONG_FLOAT" then
                  return Biggest_Int_Img (Long_Float'Size);
               elsif Pfx = "STANDARD.WIDE_CHARACTER" then
                  return "16";

               elsif Pfx = "STANDARD.WIDE_WIDE_CHARACTER" then
                  return "32";
               end if;
            end;

         when A_Variable_Declaration | A_Constant_Declaration =>
            -- Try objects's type size
            declare
               Obj_Def : constant Asis.Definition := Object_Declaration_View (Decl);
            begin
               case Definition_Kind (Obj_Def) is
                  when A_Subtype_Indication =>
                     return Size_Value_Image (First_Subtype_Name (Subtype_Simple_Name (Obj_Def)));
                  when others =>
                     -- Anonymous array type, anonymous access type, single task or protected object: give up
                     return "";
               end case;
            end;

         when A_Component_Declaration =>
            -- Is this component sized by a component clause of the enclosing record?
            declare
               Compo_Clause : constant Asis.Component_Clause := Corresponding_Component_Clause (Def);
               R            : Asis.Discrete_Range;
               L            : Extended_Biggest_Natural;
               H            : Extended_Biggest_Int;
            begin
               if Is_Nil (Compo_Clause) then
                  return "";
               end if;

               R := Component_Clause_Range (Compo_Clause);
               L := Discrete_Static_Expression_Value (Lower_Bound (R));
               H := Discrete_Static_Expression_Value (Upper_Bound (R));
               if L /= Not_Static and H /= Not_Static then
                  return Extended_Biggest_Int'Wide_Image (H - L + 1);
               else
                  return "";
               end if;
            end;

         when others =>
            null;
      end case;

      -- Bad luck...
      return "";
   end Size_Value_Image;

   ----------------
   -- Size_Value --
   ----------------

   function Size_Value (Name : Asis.Expression) return Extended_Biggest_Int is
      Str_Val : constant Wide_String := Size_Value_Image (Name);
   begin
      if Str_Val = "" then
         return Not_Static;
      end if;

      return Biggest_Int'Wide_Value (Str_Val);
   exception
      when Constraint_Error =>
         return Not_Static;
   end Size_Value;


   -----------------------------------
   -- Static_Expression_Value_Image --
   -----------------------------------

   function Static_Expression_Value_Image (Expression : Asis.Expression) return Wide_String is
      use Asis.Expressions;

      -- The function Is_Float_String returns true if, and only if, the argument
      -- text corresponds to a float.
      function Is_Float_String(Str: Wide_String) return Boolean is
      begin
         for I in Str'Range loop
            if Str(I) = '.' then
               return True;
            end if;
         end loop;
         return False;
      end Is_Float_String;

      generic
         with function Op_Int (Left, Right : Biggest_Int) return Biggest_Int;
         with function Op_Float (Left, Right : Biggest_Float) return Biggest_Float;
      function String_Arithmetic_Op (Left, Right : Wide_String) return Wide_String;
      function String_Arithmetic_Op (Left, Right : Wide_String) return Wide_String is
      begin
         if Left = "" or Right = "" then
            return "";
         end if;
         if Is_Float_String(Left) or Is_Float_String(Right) then
            return Biggest_Float'Wide_Image (Op_Float (Biggest_Float'Wide_Value (Left),
                                                       Biggest_Float'Wide_Value (Right)));
         else
            return Biggest_Int_Img (Op_Int (Biggest_Int'Wide_Value (Left),
                                            Biggest_Int'Wide_Value (Right)));
         end if;
      end String_Arithmetic_Op;

      function "+" is new String_Arithmetic_Op ("+","+");
      function "-" is new String_Arithmetic_Op ("-","-");
      function "*" is new String_Arithmetic_Op ("*","*");
      function "/" is new String_Arithmetic_Op ("/","/");
      -- Cannot do the same for "**", since Right is always Natural
      function "**" (Left, Right : Wide_String) return Wide_String is
      begin
         if Left = "" or Right = "" then
            return "";
         end if;
         if Is_Float_String(Left) then
            return Biggest_Float'Wide_Image ("**" (Biggest_Float'Wide_Value (Left),
                                                   Natural'Wide_Value (Right)));
         else
            return Biggest_Int_Img ("**" (Biggest_Int'Wide_Value (Left),
                                          Natural'Wide_Value (Right)));
         end if;
      end "**";

      Bool_Pos : constant array (Boolean) of Wide_String (1..1) := ("0", "1");
      generic
         with function Op_Int (Left, Right : Biggest_Int) return Boolean;
         with function Op_Float (Left, Right : Biggest_Float) return Boolean;
      function String_Logical_Op (Left, Right : Wide_String) return Wide_String;
      function String_Logical_Op (Left, Right : Wide_String) return Wide_String is
      begin
         if Left = "" or Right = "" then
            return "";
         end if;
         if Is_Float_String(Left) or Is_Float_String(Right) then
            return Bool_Pos (Op_Float (Biggest_Float'Wide_Value (Left),
                                       Biggest_Float'Wide_Value (Right)));
         else
            return Bool_Pos (Op_Int (Biggest_Int'Wide_Value (Left),
                                     Biggest_Int'Wide_Value (Right)));
         end if;
      end String_Logical_Op;
      function "="  is new String_Logical_Op ("=",  "=");
      function "/=" is new String_Logical_Op ("/=", "/=");
      function "<"  is new String_Logical_Op ("<",  "<");
      function "<=" is new String_Logical_Op ("<=", "<=");
      function ">"  is new String_Logical_Op (">",  ">");
      function ">=" is new String_Logical_Op (">=", ">=");


      function Strip_Underscores (Item : Wide_String) return Wide_String is
         Result : Wide_String (Item'Range);
         R_Inx  : Natural := Item'First - 1;
      begin
         for I_Inx in Item'Range loop
            if Item (I_Inx) /= '_' then
               R_Inx := R_Inx + 1;
               Result (R_Inx) := Item (I_Inx);
            end if;
         end loop;

         return Result (Result'First .. R_Inx);
      end Strip_Underscores;

      function Strip_Quotes (Item : Wide_String) return Wide_String is
         Result : Wide_String (Item'First + 1 .. Item'Last - 1);
         R_Inx  : Natural := Item'First;
         Quote  : Boolean := False;
      begin
         for I_Inx in Natural range Item'First + 1 .. Item'Last - 1 loop
            if Item (I_Inx) = '"' and not Quote then
               Quote := True;
            else
               Quote := False;
               R_Inx := R_Inx + 1;
               Result (R_Inx) := Item (I_Inx);
            end if;
         end loop;

         return Result (Result'First .. R_Inx);
      end Strip_Quotes;

      Decl : Asis.Declaration;
   begin  -- Static_Expression_Value_Image
      case Expression_Kind (Expression) is
         when An_Integer_Literal =>
            -- We make a round-trip through Value/Image below to normalize the form of the result
            -- (get rid of based numbers and '_')
            return Biggest_Int_Img (Biggest_Int'Wide_Value (Value_Image (Expression)));

         when A_Real_Literal =>
            -- We can't make the same trick as with Integer literals, since it could cause
            -- rounding issues. We just get rid of '_'. Who uses based reals anyway?
            return Strip_Underscores (Value_Image (Expression));

         when A_String_Literal =>
            return Strip_Quotes (Value_Image (Expression));

         when An_Enumeration_Literal
           | A_Character_Literal
           =>
            return Position_Number_Image (Corresponding_Name_Definition (Expression));

         when A_Parenthesized_Expression =>
            return Static_Expression_Value_Image (Expression_Parenthesized (Expression));

         when An_Identifier =>
            Decl := Corresponding_Name_Declaration (Expression);
            case Declaration_Kind (Decl) is
               when An_Integer_Number_Declaration
                 | A_Real_Number_Declaration
                 | A_Constant_Declaration
                 =>
                  return Static_Expression_Value_Image (Initialization_Expression (Decl));
               when others =>
                  return "";
            end case;

         when A_Selected_Component =>
            -- If the prefix is not the name of an enclosing construt (i.e. record selection f.e.)
            -- it will not be static anyway
            return Static_Expression_Value_Image (Selector (Expression));

         when A_Type_Conversion
           | A_Qualified_Expression
           =>
            return Static_Expression_Value_Image (Converted_Or_Qualified_Expression (Expression));

         when A_Function_Call =>
            declare
               Params  : constant Association_List := Function_Call_Parameters (Expression);
               Op_Name : constant Asis.Expression := Simple_Name (Prefix (Expression));
            begin
               case Expression_Kind (Op_Name) is
                  when An_Operator_Symbol =>
                     -- Check that the operator is the real one, not some user-defined function
                     -- For predefined operations, either there is no "fake" declaration and
                     -- Corresponding_Name_Declaration returns Nil_Element (GNAT case), or the
                     -- Declaration_Origin is An_Implicit_Predefined_Declaration.
                     Decl := Corresponding_Called_Function (Expression);
                     if not Is_Nil (Decl)
                       and then Declaration_Origin (Decl) /= An_Implicit_Predefined_Declaration
                     then
                        return "";
                     end if;

                     case Operator_Kind (Op_Name) is
                        when A_Unary_Plus_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)));

                        when A_Unary_Minus_Operator =>
                           return "0"
                             - Static_Expression_Value_Image (Actual_Parameter (Params (1)));

                        when A_Plus_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             + Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Minus_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             - Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Concatenate_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             & Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Multiply_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             * Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Divide_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             / Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when An_Exponentiate_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             ** Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when An_Equal_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             = Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Not_Equal_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             /= Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Less_Than_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             < Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Less_Than_Or_Equal_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             <= Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Greater_Than_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             > Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when A_Greater_Than_Or_Equal_Operator =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             >= Static_Expression_Value_Image (Actual_Parameter (Params (2)));

                        when others =>
                           -- Not implemented, or Not_An_Operator
                           return "";
                     end case;

                  when An_Attribute_Reference =>
                     case Attribute_Kind (Prefix (Expression)) is
                        when A_Succ_Attribute =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             + "1";
                        when A_Pred_Attribute =>
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)))
                             - "1";
                        when A_Pos_Attribute
                          | A_Val_Attribute
                          =>
                           -- Since we keep the Pos, these are no-ops
                           return Static_Expression_Value_Image (Actual_Parameter (Params (1)));
                        when others =>
                           -- Not implemented, or Not_An_Attribute
                           return "";
                     end case;

                  when others =>
                     -- A regular (user) function call
                     return "";
               end case;
            end;

         when An_Attribute_Reference =>
            case Attribute_Kind (Expression) is
               when A_First_Attribute =>
                  begin
                     declare
                        Bounds   : constant Asis.Expression_List
                          := Discrete_Constraining_Bounds (Prefix (Expression), Follow_Access => True);
                        Dim_Expr : constant Asis.Expression_List := Attribute_Designator_Expressions (Expression);
                        Dim      : ASIS_Positive;
                     begin
                        if Is_Nil (Dim_Expr) then
                           Dim := 1;
                        else
                           -- In the extremely unlikely case where the static expression Dim_Expr is
                           -- too complicated for us to evaluate, the following will raise Constraint_Error,
                           -- and thus we will return "", which is appropriate.
                           Dim := ASIS_Integer'Wide_Value (Static_Expression_Value_Image (Dim_Expr (1)));
                        end if;

                        case Element_Kind (Bounds (2 * Dim - 1)) is
                           when An_Expression =>
                              return Static_Expression_Value_Image (Bounds (2 * Dim - 1));
                           when A_Defining_Name =>
                              -- Enumeration
                              return Position_Number_Image (Bounds (2 * Dim - 1));
                           when Not_An_Element =>
                              -- Either a formal type or a modular type
                              -- But in the case of a modular type, we know damn well that 'First = 0
                              if Type_Category (Prefix (Expression)) = A_Modular_Type then
                                 return " 0";
                              else
                                 return "";
                              end if;
                           when others =>
                              Impossible ("Bad return from Discrete_Constraining_Bounds", Bounds (2 * Dim - 1));
                        end case;
                     end;
                  exception
                     when others =>
                        -- This might happen because we are querying a huge enumeration type
                        -- whose Enumeratrion_Literal_Declarations raises Storage_Error (but
                        -- at this point, it might have turned into Asis_Error, hence the when others)
                        -- In practice, most of the time, this type will be Wide_Character.
                        -- Let's give it another chance.
                        if To_Upper (Full_Name_Image (Prefix (Expression))) = "STANDARD.WIDE_CHARACTER" then
                           return " 0";
                        end if;
                        -- something else...
                        raise;
                  end;
               when A_Last_Attribute =>
                  begin
                     declare
                        Bounds   : constant Asis.Expression_List
                          := Discrete_Constraining_Bounds (Prefix (Expression), Follow_Access => True);
                        Dim_Expr : constant Asis.Expression_List := Attribute_Designator_Expressions (Expression);
                        Dim      : ASIS_Positive;
                     begin
                        if Is_Nil (Dim_Expr) then
                           Dim := 1;
                        else
                           -- In the extremely unlikely case where the static expression Dim_Expr is
                           -- too complicated for us to evaluate, the following will raise Constraint_Error,
                           -- and thus we will return "", which is appropriate.
                           Dim := ASIS_Integer'Wide_Value (Static_Expression_Value_Image (Dim_Expr (1)));
                        end if;

                        case Element_Kind (Bounds (2 * Dim - 1)) is
                           when An_Expression =>
                              return Static_Expression_Value_Image (Bounds (2 * Dim));
                           when A_Defining_Name =>
                              -- Enumeration
                              return Position_Number_Image (Bounds (2 * Dim));
                           when Not_An_Element =>
                              -- Modular type
                              return Static_Expression_Value_Image (Bounds (2 * Dim)) - "1";
                           when others =>
                              Impossible ("Bad return from Discrete_Constraining_Bounds", Bounds (2 * Dim - 1));
                        end case;
                     end;
                  exception
                     when others =>
                        -- This might happen because we are querying a huge enumeration type
                        -- whose Enumeratrion_Literal_Declarations raises Storage_Error (but
                        -- at this point, it might have turned into Asis_Error, hence the when others)
                        -- In practice, most of the time, this type will be Wide_Character.
                        -- Let's give it another chance.
                        if To_Upper (Full_Name_Image (Prefix (Expression))) = "STANDARD.WIDE_CHARACTER" then
                           return " 65535";
                        end if;
                        -- something else...
                        raise;
                  end;
               when A_Length_Attribute =>
                  declare
                     Lengths   : constant Extended_Biggest_Natural_List
                       := Discrete_Constraining_Lengths (Prefix (Expression), Follow_Access => True);
                     Dim_Expr  : constant Asis.Expression_List := Attribute_Designator_Expressions (Expression);
                     Dim       : ASIS_Positive;
                  begin
                     if Is_Nil (Dim_Expr) then
                        Dim := 1;
                     else
                        -- In the extremely unlikely case where the static expression Dim_Expr is
                        -- too complicated for us to evaluate, the following will raise Constraint_Error,
                        -- and thus we will return "", which is appropriate.
                        Dim := ASIS_Integer'Wide_Value (Static_Expression_Value_Image (Dim_Expr (1)));
                     end if;
                     if Lengths (Dim) = Not_Static then
                        return "";
                     else
                        return Extended_Biggest_Natural'Wide_Image (Lengths (Dim));
                     end if;
                  end;
               when A_Modulus_Attribute =>
                  declare
                     Type_Definition : constant Definition := Ultimate_Expression_Type (Prefix (Expression));
                  begin
                     return Static_Expression_Value_Image (Definitions.Mod_Static_Expression (Type_Definition));
                  end;
               when A_Size_Attribute =>
                  return Size_Value_Image (Simple_Name (Prefix (Expression)));
               when others =>
                  return "";
            end case;

         when An_If_Expression =>
            declare
               If_Paths : constant Asis.Path_List := Expression_Paths (Expression);
               Has_Else : constant Boolean        := Path_Kind (If_Paths (If_Paths'Last)) = An_Else_Expression_Path;
               Last     : List_Index;
            begin
               if Has_Else then
                  Last := If_Paths'Last - 1;
               else
                  Last := If_Paths'Last;
               end if;
               for P in List_Index range If_Paths'First .. Last loop
                  declare
                     Cond_Str : constant Wide_String := Static_Expression_Value_Image
                                                        (Condition_Expression (If_Paths (P)));
                  begin
                     if Cond_Str = "" then
                        -- non static condition
                        return "";
                     elsif Cond_Str = "1" then
                        return Static_Expression_Value_Image (Dependent_Expression (If_Paths (P)));
                     end if;
                  end;
               end loop;

               if Has_Else then
                  return Static_Expression_Value_Image (Dependent_Expression (If_Paths (If_Paths'Last)));
               end if;

               -- An else expression path can be omitted only if the expression is boolean, and the default is
               -- False whose 'Pos is 0
               return "0";
            end;

         when others =>
            -- Including Not_An_Expression
            return "";
      end case;

   exception
      when Constraint_Error =>
         -- Out of range of Biggest_Int f.e., give up
         return "";
   end Static_Expression_Value_Image;


   --------------------------------------
   -- Discrete_Static_Expression_Value --
   --------------------------------------

   function Discrete_Static_Expression_Value (Expression : Asis.Expression) return Extended_Biggest_Int is
      Str_Val : constant Wide_String := Static_Expression_Value_Image (Expression);
   begin
      if Str_Val = "" then
         return Not_Static;
      end if;

      return Biggest_Int'Wide_Value (Str_Val);
   exception
      when Constraint_Error =>
         return Not_Static;
   end Discrete_Static_Expression_Value;


   --------------------------
   -- Is_Static_Expression --
   --------------------------

   function Is_Static_Expression (Expr : Asis.Expression) return Boolean is
      use Asis.Expressions;

      function Are_Static_Components (Compos : Asis.Association_List) return Boolean is
      begin
         for C in Compos'Range loop
            if not Is_Static_Expression (Component_Expression (Compos (C))) then
               return False;
            end if;
         end loop;
         return True;
      end Are_Static_Components;

   begin  -- Is_Static_Expression
      case Expression_Kind (Expr) is
         when A_Record_Aggregate =>
            return Are_Static_Components (Record_Component_Associations (Expr, Normalized => True));
         when An_Extension_Aggregate =>
            return Is_Static_Expression (Extension_Aggregate_Expression (Expr))
              and then Are_Static_Components (Record_Component_Associations (Expr, Normalized => True));
         when A_Positional_Array_Aggregate
            | A_Named_Array_Aggregate
            =>
            return Are_Static_Components (Array_Component_Associations (Expr));
         when others =>
            return Static_Expression_Value_Image (Expr) /= "";
      end case;
   end Is_Static_Expression;


   -------------------------
   -- Variables_Proximity --
   -------------------------

   -- Algorithm:
   -- Build an array of descriptors for each, each representing a "part" of the name (identifier,
   -- indexing, selectors, dereferences, etc.), including implicit dereferences, then compare.
   -- Note that anything left of the rightmost dereference is irrelevant.
   function Variables_Proximity (Left, Right : Asis.Element) return Proximity is
      use Asis.Expressions;

      type Part_Kind is (Identifier, Field, Indexing, Dereference, Call, Not_Variable);
      type Name_Part (The_Kind : Part_Kind := Identifier) is
         record
            case The_Kind is
               when Identifier =>
                  Id_Name : Asis.Expression;
               when Field =>
                  Sel_Name : Asis.Expression;
               when Indexing =>
                  Indexers : Asis.Expression;
               when Dereference =>
                  Designated_Type : Asis.Definition;
               when Call =>
                  null;
               when Not_Variable =>
                  null;
            end case;
         end record;

      type Name_Descriptor is array (Positive range <>) of Name_Part;

      function Descriptor (Name : Asis.Element; With_Deref : Boolean := False) return Name_Descriptor is
         E             : Asis.Element := Name;
         Variable_Decl : Asis.Element;

         function Complete_For_Access (D : Name_Descriptor) return Name_Descriptor is
         -- Add a "Dereference" part if the *type* is an access type
         -- This allows explicit and implicit dereferences to match

            The_Type : Asis.Definition;
         begin
            if not With_Deref then
               return D;
            end if;

            The_Type := Corresponding_Expression_Type_Definition (E);
            if not Is_Access_Subtype (The_Type) then
               return D;
            end if;

            if Declaration_Kind (Enclosing_Element (The_Type))
               in An_Ordinary_Type_Declaration .. A_Subtype_Declaration
               -- All types and subtypes declarations (excludes ASIS95 handling of anonymous types)
            then
               -- unwind private types etc.
               The_Type := Type_Declaration_View (Ultimate_Type_Declaration (Enclosing_Element (The_Type)));
            end if;
            return D & Name_Part'(Dereference,
                                  Type_Declaration_View (Access_Target_Type (The_Type)));
         end Complete_For_Access;

      begin  -- Descriptor
         if Element_Kind (E) = A_Defining_Name then
            return (1 => (Identifier, E));
         end if;

         loop
            case Expression_Kind (E) is
               when An_Identifier =>
                  -- Return the "true" definion of Variable, after following all renamings
                  -- But the renaming can be a complicated expression like:
                  -- A : T renames Rec.X.Y(3);
                  Variable_Decl := Corresponding_Name_Declaration (E);
                  if Declaration_Kind (Variable_Decl) not in A_Renaming_Declaration then
                     return Complete_For_Access ((1 => (Identifier, E)));
                  end if;
                  E := Renamed_Entity (Variable_Decl);

               when A_Selected_Component =>
                  case Declaration_Kind (Corresponding_Name_Declaration (Selector (E))) is
                     when A_Component_Declaration | A_Discriminant_Specification =>
                        -- It's a record field, a protected type field...
                        return Complete_For_Access (Descriptor (Prefix (E), With_Deref => True)
                                                    & Name_Part'(Field, Selector (E)));
                     when A_Variable_Declaration
                       | An_Object_Renaming_Declaration
                       | A_Function_Body_Declaration
                       =>
                        -- Its a Pack.Var or Pack.F() selector
                        E := Selector (E);
                     when others =>
                        return (1 => (The_Kind => Not_Variable));
                  end case;

               when An_Indexed_Component =>
                  return Complete_For_Access (Descriptor (Prefix (E), With_Deref => True)
                                              & Name_Part'(Indexing, E));

               when A_Slice =>
                  -- Well, it could be the whole object as well...
                  -- Simply ignore the slice
                  -- (Too complicated to check for static matching)
                   return Complete_For_Access (Descriptor (Prefix (E), With_Deref => True));

               when A_Function_Call =>
                  --  a Function_Call can appear only as the first
                  --  element, and if it returns an access value,
                  --  or a composite object used for one of its
                  --  access subcomponents.
                  return Complete_For_Access ((1 => (The_Kind => Call)));

               when An_Explicit_Dereference =>
                  return Complete_For_Access
                    (Descriptor (Prefix (E))
                     &  Name_Part'(Dereference, Corresponding_Expression_Type_Definition (E)));

               when A_Type_Conversion =>
                  -- type conversions don't change variables, and have to be ignored for matching purpose
                  E := Converted_Or_Qualified_Expression (E);

               when others =>
                  return (1 => (The_Kind => Not_Variable));
            end case;
         end loop;
      end Descriptor;

      function Descriptors_Proximity (L_Descr, R_Descr : Name_Descriptor) return Proximity is
         -- Note that L_Descr'First and R_Descr'First are always 1
         Best_Conf : Result_Confidence;
         L_Rightmost_Deref : Natural := 1;
         R_Rightmost_Deref : Natural := 1;
         L_Inx, R_Inx : Positive;
         Base_Proximity    : Proximity;

         function Compatible_Types (L, R : Asis.Definition) return Boolean is
         -- Are L and R definitions for types in the same derivation family?
         -- We strip attributes here: T'Base and T'Class are the same as T, as far as
         -- belonging to the same derivation family is concerned.
            use Asis.Definitions;
            L_Decl, R_Decl : Asis.Declaration;
         begin
            case Definition_Kind (L) is
               when A_Subtype_Indication =>
                  L_Decl := Corresponding_Name_Declaration (Simple_Name
                                                            (Strip_Attributes
                                                             (Subtype_Simple_Name (L))));
               when A_Component_Definition =>
                  L_Decl := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                            (Component_Definition_View (L)));
               when A_Type_Definition
                  | A_Task_Definition
                  | A_Protected_Definition
                  | A_Private_Type_Definition
                  | A_Tagged_Private_Type_Definition
                  | A_Private_Extension_Definition
                  | A_Formal_Type_Definition
                  =>
                  L_Decl := Enclosing_Element (L);
                  if Declaration_Kind (L_Decl) not in A_Type_Declaration then
                     -- Definition was from an anonymous type, types are incompatible
                     return False;
                  end if;
               when An_Access_Definition =>
                  -- 2005 Consider it compatible with any access type whose target is compatible
                  if not Is_Access_Subtype (R) then
                     return False;
                  end if;
                  L_Decl := Corresponding_Name_Declaration
                             (Simple_Name
                              (Strip_Attributes
                               (Anonymous_Access_To_Object_Subtype_Mark (L))));

                  if Definition_Kind (R) = An_Access_Definition then -- 2005
                     R_Decl := Corresponding_Name_Declaration
                                (Simple_Name
                                 (Strip_Attributes
                                  (Anonymous_Access_To_Object_Subtype_Mark (R))));
                  else
                     R_Decl :=  Corresponding_Name_Declaration
                                 (Simple_Name
                                  (Strip_Attributes
                                   (Subtype_Simple_Name
                                    (Asis.Definitions.Access_To_Object_Definition (R)))));
                  end if;

                  -- Do it now to skip next case on Definition_Kind (R)
                  return Is_Equal (Ultimate_Type_Declaration (L_Decl), Ultimate_Type_Declaration (R_Decl));
               when others =>
                  Impossible ("Compatible_Types: Bad kind for L", L);
            end case;

            case Definition_Kind (R) is
               when A_Subtype_Indication =>
                  R_Decl := Corresponding_Name_Declaration
                             (Simple_Name
                              (Strip_Attributes
                               (Subtype_Simple_Name (R))));
               when A_Component_Definition =>
                  R_Decl := Corresponding_Name_Declaration
                             (Subtype_Simple_Name
                              (Component_Definition_View (R)));
               when A_Type_Definition
                  | A_Task_Definition
                  | A_Protected_Definition
                  | A_Private_Type_Definition
                  | A_Tagged_Private_Type_Definition
                  | A_Private_Extension_Definition
                  | A_Formal_Type_Definition
                  =>
                  R_Decl := Enclosing_Element (R);
                  if Declaration_Kind (R_Decl) not in A_Type_Declaration then
                     -- Definition was from an anonymous type, types are incompatible
                     return False;
                  end if;
               when An_Access_Definition =>
                  -- 2005 Consider it compatible with any access type whose target is compatible
                  -- L cannot be an access definition, because it has been checked above
                  if not Is_Access_Subtype (L) then
                     return False;
                  end if;
                  R_Decl := Corresponding_Name_Declaration
                             (Simple_Name
                              (Strip_Attributes
                               (Anonymous_Access_To_Object_Subtype_Mark (R))));

                  L_Decl :=  Corresponding_Name_Declaration
                              (Simple_Name
                               (Strip_Attributes
                                (Subtype_Simple_Name (Asis.Definitions.Access_To_Object_Definition (L)))));
               when others =>
                  Impossible ("Compatible_Types: Bad kind for R", R);
            end case;

            return Is_Equal (Ultimate_Type_Declaration (L_Decl), Ultimate_Type_Declaration (R_Decl));
         end Compatible_Types;

      begin   -- Descriptors_Proximity

         -- First, compare the "base" variable and eliminate expressions that are not variables
         for I in reverse L_Descr'Range loop
            case L_Descr (I).The_Kind is
               when Dereference =>
                  L_Rightmost_Deref := I;
                  exit;
               when Not_Variable =>
                  return Different_Variables;
               when Call =>
                  -- A function call with no dereference to its right
                  -- This cannot be an object name, => it is a value
                  return Different_Variables;
               when others =>
                  null;
            end case;
         end loop;
         for I in reverse R_Descr'Range loop
            case R_Descr (I).The_Kind is
               when Dereference =>
                  R_Rightmost_Deref := I;
                  exit;
               when Not_Variable =>
                  return Different_Variables;
               when Call =>
                  -- A function call with no dereference to its right
                  -- This cannot be an object name, => it is a value
                  return Different_Variables;
               when others =>
                  null;
            end case;
         end loop;

         if L_Rightmost_Deref = 1 and R_Rightmost_Deref = 1 then
            -- No dereference on either side
            if (L_Descr (1).The_Kind = Identifier and R_Descr (1).The_Kind = Identifier)
              and then not Is_Equal (First_Defining_Name (L_Descr (1).Id_Name),
                                     First_Defining_Name (R_Descr (1).Id_Name))
            then
               return Different_Variables;
            elsif L_Descr (1).The_Kind = Call or R_Descr (1).The_Kind = Call then
               -- Function call without dereference => it is a value, not a name
               return Different_Variables;
            end if;
            -- Here, the first element (on each side) is not a dereference;
            -- It cannot be Not_A_Variable (eliminated above)
            -- It cannot be a field or an indexing (since it is the first one)
            -- It cannot be a function call (elsif above)
            -- => It is an identifier, for the same variable

         elsif L_Rightmost_Deref /= 1 and R_Rightmost_Deref /= 1 then
            -- Dereferences on both sides
            Base_Proximity := Descriptors_Proximity (L_Descr (1 .. L_Rightmost_Deref - 1),
                                                     R_Descr (1 .. R_Rightmost_Deref - 1));
            if Base_Proximity /= Same_Variable then
               if Compatible_Types (L_Descr (L_Rightmost_Deref).Designated_Type,
                                    R_Descr (R_Rightmost_Deref).Designated_Type)
               then
                  return (Unlikely, Complete);
               else
                  return (Unlikely, Partial);
               end if;
            end if;
            -- Here, both sides are dereferences of same access variable => same target variable

         elsif L_Rightmost_Deref /= 1 then
            -- Dereference on left side only
            if Compatible_Types (L_Descr (L_Rightmost_Deref).Designated_Type,
                                 Corresponding_Expression_Type_Definition (R_Descr (1).Id_Name))
            then
               return (Unlikely, Complete);
            else
               return (Unlikely, Partial);
            end if;
         else
            -- Dereference on right side only
            if Compatible_Types (Corresponding_Expression_Type_Definition (L_Descr (1).Id_Name),
                                 R_Descr (R_Rightmost_Deref).Designated_Type)
            then
                  return (Unlikely, Complete);
            else
                  return (Unlikely, Partial);
            end if;
         end if;

         -- Here, Left and Right are the same variable
         -- Compare the rest to refine how much they overlap
         Best_Conf := Certain;
         L_Inx     := L_Rightmost_Deref + 1;
         R_Inx     := R_Rightmost_Deref + 1;
         while L_Inx <= L_Descr'Last and R_Inx <= R_Descr'Last loop
            if L_Descr (L_Inx).The_Kind /= R_Descr (R_Inx).The_Kind then
               return Different_Variables;
            end if;
            case L_Descr (L_Inx).The_Kind is
               when Identifier
                 | Not_Variable
                 =>
                  Impossible ("Variables proximity: " & Part_Kind'Wide_Image (L_Descr (L_Inx).The_Kind),
                              L_Descr (L_Inx).Id_Name);
               when Field =>
                  if not Is_Equal (Corresponding_Name_Definition (L_Descr (L_Inx).Sel_Name),
                                   Corresponding_Name_Definition (R_Descr (R_Inx).Sel_Name))
                  then
                     return Different_Variables;
                  end if;
               when Indexing =>
                  declare
                     L_Indexes : constant Asis.Expression_List := Index_Expressions (L_Descr (L_Inx).Indexers);
                     R_Indexes : constant Asis.Expression_List := Index_Expressions (R_Descr (R_Inx).Indexers);
                  begin
                     if L_Indexes'Length /= R_Indexes'Length then
                        Impossible ("Variables proximity: different lengths", Nil_Element);
                     end if;
                     for I in L_Indexes'Range loop
                        declare
                           L_Value : constant Wide_String := Static_Expression_Value_Image (L_Indexes (I));
                           R_Value : constant Wide_String := Static_Expression_Value_Image (R_Indexes (I));
                        begin
                           if L_Value = "" or R_Value = "" then
                              Best_Conf := Result_Confidence'Min (Best_Conf, Possible);
                           elsif L_Value /= R_Value then
                              return Different_Variables;
                           end if;
                        end;
                     end loop;
                  end;
               when Dereference =>
                  Impossible ("Variables proximity: dereference", Nil_Element);
               when Call =>
                  Impossible ("Variables proximity: call", Nil_Element);
            end case;

            L_Inx := L_Inx + 1;
            R_Inx := R_Inx + 1;
         end loop;

         if L_Inx = L_Descr'Last + 1 and R_Inx = R_Descr'Last + 1 then
            return (Best_Conf, Complete);
         else
            return (Best_Conf, Partial);
         end if;
      end Descriptors_Proximity;

   begin  -- Variables_Proximity
      return Descriptors_Proximity (Descriptor (Left), Descriptor (Right));
   end Variables_Proximity;

   ----------------
   -- Same_Value --
   ----------------

   function Same_Value (Left, Right : Asis.Expression) return Boolean is
      Good_Left, Good_Right : Asis.Expression;

      function Same_Constant (Left_C, Right_C : Asis.Expression) return Boolean is
         -- Precondition: Left_C and Right_C are An_Identifier
         use Asis.Expressions;

         Decl : constant Asis.Declaration := Corresponding_Name_Declaration (Left_C);
         Left_Obj  : Asis.Expression;
         Right_Obj : Asis.Expression;
      begin
         case Declaration_Kind (Decl) is
            when A_Constant_Declaration =>
               return Is_Equal (Corresponding_Name_Definition (Left_C),
                                Corresponding_Name_Definition (Right_C));
            when A_Parameter_Specification =>
               if Mode_Kind (Decl) in A_Default_In_Mode .. An_In_Mode then
                  return Is_Equal (Corresponding_Name_Definition (Left_C),
                                   Corresponding_Name_Definition (Right_C));
               else
                  return False;
               end if;
            when An_Object_Renaming_Declaration =>
               Left_Obj  := Ultimate_Name (Left_C,  No_Component => True);
               Right_Obj := Ultimate_Name (Right_C, No_Component => True);
               if Is_Nil (Left_Obj) or Is_Nil (Right_Obj) then
                  -- Dynamic renaming...
                  return False;
               end if;
               if Same_Constant (Left_Obj, Right_Obj) then
                  -- We have the same constant objects, but for records we must make sure
                  -- that the renamings target the same components. The following test duplicates
                  -- the previous one if it is NOT the renaming of a component, but what's the heck..
                  return Is_Equal (Corresponding_Name_Definition (Ultimate_Name (Left_C,  No_Component => False)),
                                   Corresponding_Name_Definition (Ultimate_Name (Right_C, No_Component => False)));
               else
                  return False;
               end if;
            when others =>
               return False;
         end case;
      end Same_Constant;
   begin   -- Same_Value
      Good_Left  := Simple_Name (Left);
      Good_Right := Simple_Name (Right);

      if Expression_Kind (Good_Left) = An_Identifier
        and then Expression_Kind (Good_Right) = An_Identifier
        and then Same_Constant (Good_Left, Good_Right)
      then
         return True;
      end if;

      -- Here, Left and Right do not refer to the same constant or "in" parameter
      declare
         Left_Value : constant Wide_String := Static_Expression_Value_Image (Left);
         Right_Value :constant Wide_String := Static_Expression_Value_Image (Right);
      begin
         return Left_Value /= "" and then Left_Value = Right_Value;
      end;
   end Same_Value;

   ------------------
   -- Static_Level --
   ------------------

   function Static_Level (Element : Asis.Element) return Asis.ASIS_Natural is
      use Asis.Expressions;

      Decl : Asis.Element;
      Result : ASIS_Natural := 0;
   begin
      case Element_Kind (Element) is
         when A_Defining_Name =>
            Decl := Enclosing_Element (Enclosing_Element (Element));
         when A_Declaration =>
            Decl := Enclosing_Element (Element);
         when An_Expression =>
            Decl := Enclosing_Element (Corresponding_Name_Declaration (Simple_Name (Element)));
         when others =>
            Impossible ("Bad element kind in Static_Level", Element);
      end case;

      while not Is_Nil (Decl) loop
         case Element_Kind (Decl) is
            when A_Declaration =>
               case Declaration_Kind (Decl) is
                  when A_Task_Type_Declaration
                     | A_Single_Task_Declaration
                     | A_Procedure_Declaration
                     | A_Null_Procedure_Declaration   -- Ada 2005
                     | A_Function_Declaration
                     | An_Expression_Function_Declaration   -- Ada 2012
                     | A_Procedure_Body_Declaration
                     | A_Function_Body_Declaration
                     | A_Task_Body_Declaration
                     | An_Entry_Body_Declaration
                     | A_Generic_Declaration
                     =>
                     Result := Result + 1;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Decl := Enclosing_Element (Decl);
      end loop;

      return Result;
   end Static_Level;


   ----------------------
   -- Strip_Attributes --
   ----------------------

   function Strip_Attributes (Name : Asis.Expression) return Asis.Expression is
      use Asis.Expressions;
      Result : Asis.Expression := Name;
   begin
      while Expression_Kind (Result) = An_Attribute_Reference loop
         Result := Prefix (Result);
      end loop;
      return Result;
   end Strip_Attributes;


   -----------------------
   -- Strip_Parentheses --
   -----------------------

   function Strip_Parentheses (Expr : Asis.Expression) return Asis.Expression is
      use Asis.Expressions;

      Result : Asis.Expression := Expr;
   begin
      while Expression_Kind (Result) = A_Parenthesized_Expression loop
         Result := Expression_Parenthesized (Result);
      end loop;
      return Result;
   end Strip_Parentheses;


   ----------------------
   -- Used_Identifiers --
   ----------------------

   function Used_Identifiers (Name : Asis.Expression) return Asis.Expression_List is
      Decl : Asis.Declaration;

      function Expression_Used_Identifiers (Expr : Asis.Expression) return Asis.Expression_List is
      -- Builds the list of used identifiers in the expression of an object renaming by scanning
      -- Expr right to left, until we find a name that is not used, nor any of its predecessors
      -- (like a package name, a dereference...)
      -- This function could be (partly) iterative, but making it recursive avoids to deal with
      -- the number of elements of the results, and the call depth should be quite small in practice.
         use Asis.Expressions;
         Pref : Asis.Expression;
      begin
         case Expression_Kind (Expr) is
            when An_Identifier
               | An_Operator_Symbol
               | An_Enumeration_Literal
               | A_Character_Literal
                 =>
               return Used_Identifiers (Expr);
            when An_Attribute_Reference =>
               -- The prefix of an attribute is evaluated at the place of the renaming
               -- not when using the renamed entity
               return (1 => Expr);
            when A_Selected_Component =>
               Pref := Prefix (Expr);
               if Is_Access_Expression (Pref) then
                  -- Implicit dereference
                  -- everything left of the dereference is "used" at the place of the renaming
                  -- not when using the renamed entity
                  return Used_Identifiers (Selector (Expr));
               else
                  return Expression_Used_Identifiers (Pref) & Used_Identifiers (Selector (Expr));
               end if;
            when An_Explicit_Dereference =>
               -- The dereference is "used" at the place of the renaming
               -- not when using the renamed entity
               return Nil_Element_List;
            when A_Slice
               | An_Indexed_Component
                 =>
               -- The indexing expression is "used" at the place of the renanming,
               -- not when using the renamed entity
               Pref := Prefix (Expr);
               if Is_Access_Expression (Pref) then
                  -- Implicit dereference
                  -- everything left of the dereference is "used" at the place of the renaming
                  -- not when using the renamed entity
                  return Nil_Element_List;
               else
                  return Expression_Used_Identifiers (Pref);
               end if;
            when A_Function_Call =>
               -- The function is "used" at the place of the renanming,
               -- not when using the renamed entity
               return Nil_Element_List;
            when A_Type_Conversion =>
               return Expression_Used_Identifiers (Converted_Or_Qualified_Expression (Expr));
            when others =>
               Impossible ("Expression_Used_Identifiers: unexpected expression in renaming", Expr);
         end case;
      end Expression_Used_Identifiers;

      use Asis.Expressions;
   begin -- Used_Identifiers
      Decl := Corresponding_Name_Declaration (Name);
      if Declaration_Kind (Decl) in A_Renaming_Declaration then
         return Name & Expression_Used_Identifiers (Renamed_Entity (Decl));
      else
         -- including Not_A_Declaration for predefined stuff
         return (1 => Name);
      end if;
   end Used_Identifiers;

end Thick_Queries;
