----------------------------------------------------------------------
--  Thick_Queries - Package body                                    --
--  Copyright (C) 2002 Adalog                                       --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                   Tel: +33 1 41 24 31 40                 --
--  19-21 rue du 8 mai 1945  Fax: +33 1 41 24 07 36                 --
--  94110 ARCUEIL            E-m: info@adalog.fr                    --
--  FRANCE                   URL: http://www.adalog.fr              --
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
with   -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed;

with   -- Reusable components
  A4G_Bugs;

with   -- ASIS units
  Asis.Clauses,
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements,
  Asis.Expressions,
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
   pragma No_Return (Impossible);

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

   --------------------------
   -- Attribute_Name_Image --
   --------------------------

   function Attribute_Name_Image (Attribute : Asis.Expression) return Wide_String is
      use Asis.Expressions;
   begin
      return Name_Image (A4G_Bugs.Attribute_Designator_Identifier (Attribute));
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

      if Expression_Kind (Result) = An_Explicit_Dereference
        or else Expression_Type_Kind (Result) = An_Access_Type_Definition
      then
         Result := Nil_Element;
      end if;

      return Result;
   end Called_Simple_Name;

   --------------------
   -- Called_Profile --
   --------------------

   function Called_Profile (Call : Asis.Element) return Asis.Parameter_Specification_List is
      use Asis.Expressions, Asis.Definitions;
      Callee : Asis.Declaration;
      Name   : Asis.Expression;
   begin
      if Expression_Kind (Call) = A_Function_Call then
         Callee := A4G_Bugs.Corresponding_Called_Function (Call);
      else
         Callee := A4G_Bugs.Corresponding_Called_Entity (Call);
      end if;

      if Is_Nil (Callee) then
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

         if Expression_Type_Kind (Name) = An_Access_Type_Definition then
            -- access to subprogram (or entry)
            return Access_To_Subprogram_Parameter_Profile (Ultimate_Expression_Type (Name));

         else
            -- Dispatching Call, or call to a subprogram defined by an attribute (see Corresponding_Called_Entity
            -- in the ASIS specification).
            -- We have no way to get a profile. Sigh...
            return Nil_Element_List;
         end if;

      else
         if Declaration_Kind (Callee) in A_Procedure_Instantiation .. A_Function_Instantiation then
            Callee := Corresponding_Declaration (Callee);
         end if;

         return Parameter_Profile (Callee);
      end if;
   end Called_Profile;

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
               Formal_Part : Asis.Element_List
                 :=  Generic_Formal_Part (Corresponding_Name_Declaration (Ultimate_Name (Generic_Unit_Name (Call))));
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
   begin
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
   begin
      case Declaration_Kind (Call_Or_Instantiation) is
         when A_Generic_Instantiation
           | A_Formal_Package_Declaration
           =>
            declare
               Assoc_List : constant Asis.Association_List := Generic_Actual_Part (Call_Or_Instantiation);
            begin
               for I in Assoc_List'Range loop
                  if Is_Equal (Assoc_List (I), Assoc) then
                     return Formal_Name (Call_Or_Instantiation, I);
                  end if;
               end loop;
            end;



         when others =>
            declare
               Assoc_List : constant Asis.Association_List :=  Called_Profile (Call_Or_Instantiation);
            begin
               for I in Assoc_List'Range loop
                  if Is_Equal (Assoc_List (I), Assoc) then
                     return Formal_Name (Call_Or_Instantiation, I);
                  end if;
               end loop;
            end;
      end case;

      -- Index must be found, by construction
      Impossible ("Association not found in association list", Assoc);
   end Formal_Name;

   -----------------------
   -- Actual_Expression --
   -----------------------

   function Actual_Expression (Call : Asis.Element; Formal : Asis.Defining_Name) return Asis.Expression is
      use Asis.Expressions;
      Actuals : constant Asis.Association_List := Actual_Parameters (Call);
   begin
      if Is_Dispatching_Call (Call) then
         return Nil_Element;
      end if;

      for I in Actuals'Range loop
         if Is_Equal (Formal, Formal_Name (Call, I)) then
            return Actual_Parameter (Actuals (I));
         end if;
      end loop;

      -- Not found, is it a default value?
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

      My_Enclosing_Element := Enclosing_Element(Element);
      loop
         case Element_Kind (My_Enclosing_Element) is
            when Not_An_Element =>
               Result := Nil_Element;
               exit;

            when A_Declaration =>
               case Declaration_Kind (My_Enclosing_Element) is
                  when A_Procedure_Declaration
                    | A_Procedure_Body_Declaration
                    --
                    | A_Function_Declaration
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
                     Result := Names(My_Enclosing_Element)(1);
                     exit;

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

   end Enclosing_Program_Unit;


   --------------------------
   -- Expression_Type_Kind --
   --------------------------

   function Expression_Type_Kind (The_Element : Asis.Expression) return Asis.Type_Kinds is
      The_Type : constant Asis.Definition := Ultimate_Expression_Type (The_Element);
   begin
      if Is_Nil (The_Type) then
         return Not_A_Type_Definition;
      else
         return Type_Kind (The_Type);
      end if;
   end Expression_Type_Kind;


   ---------------------------
   -- Expression_Usage_Kind --
   ---------------------------

   function Expression_Usage_Kind (Expr : Asis.Expression) return Expression_Usage_Kinds is
      use Asis.Clauses, Asis.Expressions;
      use Thick_Queries;
      Elem     : Asis.Element := Enclosing_Element (Expr);
      Previous : Asis.Element := Expr;
   begin
      -- Protected objects can only be read, first get rid of that special case:
      if Definition_Kind (Ultimate_Expression_Type (Expr)) = A_Protected_Definition then
         return Read;
      end if;

      -- Go up the expression until we find something that allows us to make a decision
      loop
         case Element_Kind (Elem) is
            when An_Expression =>
               case Expression_Kind (Elem) is
                  when A_Selected_Component =>
                     if Is_Equal (Previous, Prefix (Elem) )
                       and then Expression_Type_Kind (Previous) = An_Access_Type_Definition
                     then
                        -- We have the prefix of an implicit dereference
                        -- => it is actually a Read of the variable
                        return Read;
                     else
                        Previous := Elem;
                        Elem     := Enclosing_Element (Elem);
                     end if;

                  when An_Identifier =>
                     Impossible ("enclosing element is an identifier", Elem);

                  when An_Indexed_Component
                    | A_Slice
                    =>
                     if Is_Equal (Previous, Prefix (Elem)) then
                        -- Previous is the prefix
                        Previous := Elem;
                        Elem     := Enclosing_Element (Elem);
                     else
                        -- Previous is part of the indexing or of the slice
                        return Read;
                     end if;

                  when A_Type_Conversion
                    | A_Qualified_Expression
                    =>
                     Previous := Elem;
                     Elem     := Enclosing_Element (Elem);

                  when An_Explicit_Dereference
                    | A_Function_Call =>
                     -- Explicit dereference => the object is not modified
                     -- Function call =>
                     --   if it is in a parameter, it is read (functions have only "in" parameters!)
                     --   if it is part of the function name, it is an implicit dereference if it is a variable,
                     --   otherwise it is not a variable
                     return Read;

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
                     -- Find the position
                     declare
                        Actuals : constant Asis.Association_List := Call_Statement_Parameters (Elem);
                        Formal  : Asis.Defining_Name;
                     begin
                        for I in Actuals'Range loop
                           if Is_Equal (Actuals (I), Previous) then
                              Formal := Formal_Name (Elem, I);
                              if Is_Nil (Formal) then
                                 -- Call to a dispatching operation
                                 -- We don't know the mode => pretend we do nothing
                                 -- (consistent with the fact that dispatching calls are ignored)
                                 return Untouched;
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
                     end;

                  when others =>
                     return Read;
               end case;

            when An_Association =>
               Previous := Elem;
               Elem     := Enclosing_Element (Elem);

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

   function Extended_Name_Image (Name_Elem : Asis.Element) return Wide_String is
      use Asis.Expressions;
   begin
      case Element_Kind (Name_Elem) is
         when An_Expression =>
            case Expression_Kind (Name_Elem) is
               when A_Selected_Component =>
                  return Extended_Name_Image (Prefix (Name_Elem)) & '.'
                    & Name_Image (Selector (Name_Elem));
               when An_Identifier =>
                  return Name_Image (Name_Elem);
               when others =>
                  Impossible ("Not a name in Extended_Name_Image", Name_Elem);
            end case;
         when A_Defining_Name =>
            return Defining_Name_Image (Name_Elem);
         when others =>
            Impossible ("Not a name in Extended_Name_Image", Name_Elem);
      end case;
   end Extended_Name_Image;


   --------------------------
   -- External_Call_Target --
   --------------------------

   function External_Call_Target (Call : Asis.Element) return Asis.Expression is
      use Asis.Expressions;
      Name     : Asis.Expression;
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

      if Expression_Kind (Name) = A_Selected_Component then
         case Definition_Kind (Ultimate_Expression_Type (Prefix (Name))) is
            when A_Task_Definition | A_Protected_Definition =>
               return Prefix (Name);
            when others =>
               return Nil_Element;
         end case;
      else
         -- Certainly not an external call
         return Nil_Element;
      end if;

   end External_Call_Target;

   ---------------------
   -- Full_Name_Image --
   ---------------------

   function Full_Name_Image (The_Name     : in Asis.Element;
                             With_Profile : in Boolean := False) return Wide_String is
      use Ada.Strings.Wide_Fixed, Asis.Expressions;

      Parent          : Element;
      Anonymous_Count : Natural;
      Up_Count        : Natural;
      Decl_Name       : Asis.Defining_Name;

      function Simple_Name_Image (N : Asis.Defining_Name) return Wide_String is
         -- Adds profile to name if necessary
         Name_Image : constant Wide_String := Defining_Name_Image (N);
      begin
         if With_Profile and then
           -- A generic is not overloadable, therefore we don't add the profile
           Declaration_Kind (Enclosing_Element (N)) not in A_Generic_Declaration
         then
            return Name_Image & Profile_Image (N, With_Profile => True);
         else
            return Name_Image;
         end if;
      end Simple_Name_Image;

      Decl_Name_Enclosing : Asis.Element;
   begin   -- Full_Name_Image
      if Element_Kind (The_Name) = A_Defining_Name then
         Decl_Name := The_Name;
      elsif Expression_Kind (The_Name) = A_Selected_Component then
         Decl_Name := Corresponding_Name_Definition (Selector (The_Name));
      else
         Decl_Name := Corresponding_Name_Definition (The_Name);
      end if;

      -- Get rid of (annoying) special case:
      -- A defining name that is actually part of a Defining_Expanded_Name
      -- (i.e. the composite name of a child unit).
      -- The full name is actually the Defining_Name of the enclosing construct
      Decl_Name_Enclosing := Enclosing_Element (Decl_Name);
      if Element_Kind (Decl_Name_Enclosing) = A_Defining_Name and then
        Defining_Name_Kind (Decl_Name_Enclosing) = A_Defining_Expanded_Name
      then
         return Simple_Name_Image (Decl_Name_Enclosing);
      end if;

      -- Set Up_Count, the number of enclosing elements we have to go up
      -- to reach the real parent (the unit whose name is used to qualify Decl_Name)
      if Is_Part_Of_Implicit (Decl_Name) then
         if Defining_Name_Kind (Decl_Name) = A_Defining_Operator_Symbol then
            -- Implicit operator: (is this a A4G bug?), the definition is *not* enclosed in the
            -- derived type definition as in the normal case below
            Up_Count := 2;
         else
            --  Implicit element: The name is enclosed in a subprogram declaration, whose
            --  enclosing element is the derived type definition, whose enclosing element
            --  is the derived type declaration, whose enclosing element is the parent (Ouch).
            --  See the comment for Corresponding_Name_Definition in Asis.Expressions.
            Up_Count := 4;
         end if;
      elsif Declaration_Kind (Enclosing_Element (Decl_Name)) = An_Enumeration_Literal_Specification then
         -- Enumeration literal: The name is enclosed in an enumeration_literal_specification,
         -- whose enclosing element is the type definition, whose enclosing element is the type
         -- declaration, whose enclosing element is the parent (Ouch again).
         Up_Count := 4;
      else
         -- Normal case: The name is enclosed in a declaration or statement, whose enclosing
         -- element is the parent
         Up_Count := 2;
      end if;

      Parent := Decl_Name;
      for I in Natural range 1 .. Up_Count loop
         Parent := Enclosing_Element (Parent);
      end loop;

      -- There are cases of nested definitions => go up one (or more) levels
      -- until we find something that's the "real" parent.
      --
      -- We must skip declarations that are instantiations (i.e. if we have an instantiation of
      -- a generic package, the package declaration is enclosed in a package instantiation; the actual
      -- parent is the parent of the instantiation).
      -- Things we may encounter (and skip) are paths (they do not correspond to any (terminal) Ada
      -- element) and exception handlers (case of when E: others =>)
      -- If we encounter unnamed loops or blocks, we count them, but continue to go up. This allows
      -- generating a junk name that includes as many "_anonymous_." as unnamed statements

      Anonymous_Count := 0;
      loop
         case Element_Kind (Parent) is
            when Not_An_Element =>
               -- No parent => compilation unit
               -- But can still be a proper body
               if Is_Subunit (Enclosing_Element (Decl_Name)) then
                  -- The full name is the same as the name of the stub
                  return Full_Name_Image (Names (Corresponding_Body_Stub
                                                 (Enclosing_Element (Decl_Name)))(1),
                                          With_Profile);
               else
                  return Simple_Name_Image (Decl_Name);
               end if;

            when A_Declaration =>
               case Declaration_Kind (Parent) is
                  when A_Generic_Instantiation
                    | A_Formal_Package_Declaration
                    | A_Formal_Package_Declaration_With_Box
                    =>
                     null;
                  when others =>
                     return Full_Name_Image (Names (Parent) (1), With_Profile)
                       & '.'
                       & Anonymous_Count * "_anonymous_."
                       & Simple_Name_Image (Decl_Name);
               end case;

            when A_Statement =>
               case Statement_Kind (Parent) is
                  when  A_Loop_Statement .. A_Block_Statement =>
                     -- Statements that can have a name
                     if Is_Nil (Statement_Identifier (Parent)) then
                        Anonymous_Count := Anonymous_Count + 1;
                     else
                        return Full_Name_Image (Statement_Identifier (Parent), With_Profile)
                          & '.'
                          & Anonymous_Count * "_anonymous_."
                          & Simple_Name_Image (Decl_Name);
                     end if;
                  when An_Accept_Statement =>
                     return Full_Name_Image (Names (Corresponding_Entry (Parent))(1), With_Profile)
                       & '.'
                       & Simple_Name_Image (Decl_Name);
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

      Element_Definition  : Asis.Definition;
   begin
      if Is_Nil (Element) then
         return (others => False);
      end if;

      case Element_Kind (Element) is
         when A_Defining_Name =>
            Element_Definition  := Element;

         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier =>
                  Element_Definition  := Corresponding_Name_Definition (Element);
               when A_Selected_Component =>
                  Element_Definition  := Corresponding_Name_Definition (Prefix (Element));
                  -- Expression is neither An_Identifier, nor A_Selected_Component
               when others =>
                  return (others => False);
            end case;

            -- Element is neither A_Defining_Name, nor An_Identifier, nor A_Selected_Component
         when others =>
            return (others => False);
      end case;

      declare
         Element_Pragmas : constant Asis.Pragma_Element_List := Corresponding_Pragmas (Enclosing_Element
                                                                                       (Element_Definition));
         Result : Pragma_Set := (others => False);
      begin
         for Pragma_Elt in Element_Pragmas'Range loop
            -- Retrieve the associations composing the current pragma
            declare
               Pragma_Associations : constant Asis.Association_List := Pragma_Argument_Associations (Element_Pragmas
                                                                                                     (Pragma_Elt));
            begin
               for Pragma_Assoc in Pragma_Associations'Range loop
                  -- Check if the pragma has been set on Element
                  if Is_Equal (Element_Definition,
                               Corresponding_Name_Definition (Actual_Parameter
                                                                       (Pragma_Associations (Pragma_Assoc))))
                  then
                     Result (Pragma_Kind (Element_Pragmas (Pragma_Elt))) := True;
                  end if;
               end loop;
            end;
         end loop;

         -- For variables and constants, check pragmas inherited from the type
         case Declaration_Kind (Enclosing_Element (Element_Definition)) is
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
                        Result := Result or Corresponding_Pragma_Set (Names
                                                                      (Corresponding_First_Subtype
                                                                       (Corresponding_Name_Declaration
                                                                        (Subtype_Simple_Name
                                                                         (Component_Subtype_Indication
                                                                          (Element_Type_Definition)))))(1));
                     when A_Subtype_Indication =>
                        Result := Result or Corresponding_Pragma_Set (Names
                                                                      (Corresponding_First_Subtype
                                                                       (Corresponding_Name_Declaration
                                                                        (Subtype_Simple_Name
                                                                         (Element_Type_Definition))))(1));

                     when others =>
                        null;
                  end case;
               end;
            when others =>
               null;
         end case;
         return Result;
      end;
   end Corresponding_Pragma_Set;

   ---------------------------
   -- Is_Callable_Construct --
   ---------------------------

   function Is_Callable_Construct (Element : Asis.Element) return Boolean is
      use Asis.Expressions;
      The_Declaration : Asis.Element;
   begin
      -- Go to the declaration
      case Element_Kind (Element) is
         when A_Declaration =>
            The_Declaration := Element;
         when A_Defining_Name | A_Definition =>
            The_Declaration := Enclosing_Element (Element);
            if Declaration_Kind (The_Declaration) in A_Generic_Instantiation then
               -- For a generic instantiation, take the declaration from the generic
               -- Beware: the generic name can be a selected component
               The_Declaration := Generic_Unit_Name (The_Declaration);
               if Expression_Kind (The_Declaration) = A_Selected_Component then
                  The_Declaration := Selector (The_Declaration);
               end if;
               The_Declaration := Corresponding_Name_Declaration (The_Declaration);
            end if;
         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier =>
                  The_Declaration := Corresponding_Name_Declaration (Element);
               when A_Selected_Component =>
                  The_Declaration := Corresponding_Name_Declaration (Selector (Element));
               when others =>
                  Impossible ("Is_Callable_Construct called on "
                              & Element_Kinds'Wide_Image (Element_Kind (Element)),
                              Element);
            end  case;
         when others =>
            -- Impossible
            Impossible ("Is_Callable_Construct called on "
                        & Element_Kinds'Wide_Image (Element_Kind (Element)),
                        Element);
      end case;

      case Declaration_Kind (The_Declaration) is
         when
           A_Procedure_Declaration |
           A_Procedure_Body_Declaration |
           A_Procedure_Renaming_Declaration |
           A_Procedure_Body_Stub |
           A_Generic_Procedure_Declaration |
           A_Formal_Procedure_Declaration |
           --
           A_Function_Declaration |
           A_Function_Body_Declaration |
           A_Function_Renaming_Declaration |
           A_Function_Body_Stub |
           A_Generic_Function_Declaration |
           A_Formal_Function_Declaration |
           --
           An_Enumeration_Literal_Specification |
           --
           An_Entry_Declaration |
           An_Entry_Body_Declaration =>

            return True;
         when others =>
            return False;
      end case;
   end Is_Callable_Construct;


   -------------------------
   -- Subtype_Simple_Name --
   -------------------------

   function Subtype_Simple_Name (Definition : Asis.Definition) return Asis.Expression is
      Result : constant Asis.Expression
        := Asis.Definitions.Subtype_Mark (Definition); --##RULE LINE OFF Use_Subtype_Simple_Name
   begin
      if Expression_Kind (Result) = A_Selected_Component then
         return Asis.Expressions.Selector (Result);
      else
         return Result;
      end if;
   end Subtype_Simple_Name;


   ---------------------------
   -- Is_Class_Wide_Subtype --
   ---------------------------

   function Is_Class_Wide_Subtype (The_Subtype : Asis.Declaration) return Boolean is
      ST : Asis.Declaration := The_Subtype;
   begin
      -- We must unwind subtypes up to the last subtype (but not up to the type as
      -- Corresponding_First_Subtype would do), and check if the subtype mark is a
      -- 'Class attribute.
      while Declaration_Kind (A4G_Bugs.Corresponding_Last_Subtype (ST)) = A_Subtype_Declaration loop
         ST := A4G_Bugs.Corresponding_Last_Subtype (ST);
      end loop;

      return A4G_Bugs.Attribute_Kind (Subtype_Simple_Name (Type_Declaration_View (ST))) = A_Class_Attribute;
   end Is_Class_Wide_Subtype;


   ----------------
   -- Is_Limited --
   ----------------

   function Is_Limited (The_Subtype : Asis.Declaration) return Boolean is
      use Asis.Definitions, Asis.Expressions;

      function Has_Limited_Components (Def : Asis.Definition) return Boolean is
         -- Expected Definition_Kinds:
         --   A_Record_Definition
         --   A_Null_Record_Definition
         --   A_Variant
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
                     if Is_Limited (Corresponding_Name_Declaration
                                    (Subtype_Simple_Name
                                     (Component_Subtype_Indication
                                      (Object_Declaration_View (Components (I))))))
                     then
                        return True;
                     end if;
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

      Decl : Asis.Declaration  := The_Subtype;
      Name : Asis.Expression;
   begin
      loop
         case Declaration_Kind (Decl) is
            when A_Private_Type_Declaration =>
               return Trait_Kind (Decl) = A_Limited_Private_Trait;

            when A_Private_Extension_Declaration =>
               -- A private extension is limited iff the parent type is limited
               Decl := Corresponding_Name_Declaration (Subtype_Simple_Name
                                                         (Ancestor_Subtype_Indication
                                                            (Type_Declaration_View (Decl))));
            when An_Ordinary_Type_Declaration =>
               case Type_Kind (Type_Declaration_View (Decl)) is
                  when A_Record_Type_Definition =>
                     if Trait_Kind (Decl) = A_Limited_Trait then
                        return True;
                     end if;
                     -- A record type is limited if any component is limited...
                     return Has_Limited_Components (Asis.Definitions.Record_Definition (Type_Declaration_View (Decl)));
                  when A_Tagged_Record_Type_Definition =>
                     -- Tagged record types must be declared limited if they have limited components
                     --   => no need to check subcomponents
                     return Trait_Kind (Decl) = A_Limited_Trait;
                  when A_Derived_Type_Definition =>
                     Name := Subtype_Simple_Name (Parent_Subtype_Indication (Type_Declaration_View (Decl)));
                     if Expression_Kind (Name) = An_Attribute_Reference then
                        -- T'Base or T'Class
                        Name := Prefix (Name);
                        if Expression_Kind (Name) = A_Selected_Component then
                           Name := Selector (Name);
                        end if;
                     end if;
                     Decl := Corresponding_First_Subtype (Corresponding_Name_Declaration (Name));
                  when A_Derived_Record_Extension_Definition =>
                     -- A record extension is limited iff the (ultimate) parent type is limited
                     Decl := A4G_Bugs.Corresponding_Root_Type (Type_Declaration_View (Decl));
                  when others =>
                     return False;
               end case;

            when A_Task_Type_Declaration | A_Protected_Type_Declaration =>
               return True;

            when others =>
               return False;
         end case;
      end loop;
   end Is_Limited;


   -------------------------------
   -- Ultimate_Type_Declaration --
   -------------------------------

   function Ultimate_Type_Declaration (The_Subtype : Asis.Declaration) return Asis.Declaration is
      Decl : Asis.Declaration := The_Subtype;
   begin
      loop
         case Declaration_Kind (Decl) is
            when An_Ordinary_Type_Declaration =>
               if Type_Kind (Type_Declaration_View (Decl))
                 in A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
               then
                  Decl := A4G_Bugs.Corresponding_Root_Type (Type_Declaration_View (Decl));
               else
                  return Decl;
               end if;
            when A_Task_Type_Declaration
              | A_Protected_Type_Declaration
              | A_Formal_Type_Declaration
              =>
               return Decl;
            when A_Private_Type_Declaration
              | A_Private_Extension_Declaration
              | An_Incomplete_Type_Declaration
              =>
               Decl := Corresponding_Type_Declaration (Decl);
            when A_Subtype_Declaration =>
               Decl := Corresponding_First_Subtype (Decl);
            when others =>
               Impossible ("Ultimate_Type_Declaration: bad kind", Decl);
         end case;
      end loop;
   end Ultimate_Type_Declaration;


   ------------------------------
   -- Is_Type_Declaration_Kind --
   ------------------------------

   function Is_Type_Declaration_Kind (The_Subtype : Asis.Declaration; Kind : Asis.Declaration_Kinds) return Boolean is
      Decl : Asis.Declaration := The_Subtype;
   begin
      Decl := Corresponding_First_Subtype (Decl);
      if Type_Kind (Type_Declaration_View (Decl)) = A_Derived_Type_Definition then
         Decl := A4G_Bugs.Corresponding_Root_Type (Type_Declaration_View (Decl));
      end if;
      return Declaration_Kind (Decl) = Kind;
   end Is_Type_Declaration_Kind;


   ------------------------------------
   -- Contains_Type_Declaration_Kind --
   ------------------------------------

   function Contains_Type_Declaration_Kind (The_Subtype : Asis.Declaration;
                                            Kind        : Asis.Declaration_Kinds) return Boolean is
      use Asis.Definitions, Asis.Expressions;

      function Components_Contain_Type_Declaration_Kind (Components : Asis.Record_Component_List) return Boolean is
         Name : Asis.Expression;
      begin
         for I in Components'Range loop
            case Element_Kind (Components (I)) is
               when A_Declaration =>
                  -- A_Component_Declaration
                  Name := Subtype_Simple_Name (Component_Subtype_Indication (Object_Declaration_View (Components (I))));
                  if Expression_Kind (Name) = An_Attribute_Reference then
                     -- A record component can't be 'Class, must be 'Base
                     Name := Prefix (Name);
                     if Expression_Kind (Name) = A_Selected_Component then
                        Name := Selector (Name);
                     end if;
                  end if;
                  if Contains_Type_Declaration_Kind (Corresponding_Name_Declaration (Name), Kind) then
                     return True;
                  end if;
               when A_Definition =>
                  if Definition_Kind (Components (I)) = A_Variant_Part then
                     declare
                        V_List : constant Asis.Variant_List := Variants (Components (I));
                     begin
                        for J in V_List'Range loop
                           if Components_Contain_Type_Declaration_Kind (Record_Components (V_List (J))) then
                              return True;
                           end if;
                        end loop;
                     end;
                  -- else it is A_Null_Component
                  end if;
               when others =>
                  -- pragma, clause
                  null;
            end case;
         end loop;

         return False;
      end Components_Contain_Type_Declaration_Kind;

      function Discriminants_Contain_Type_Declaration_Kind (Discr_Part : Asis.Definition) return Boolean is
      begin
         if Is_Nil (Discr_Part) or else Definition_Kind (Discr_Part) = An_Unknown_Discriminant_Part then
            return False;
         end if;

         declare
            Discrs : constant Asis.Discriminant_Specification_List := Discriminants (Discr_Part);
            SM     : Expression;
         begin
            for I in Discrs'Range loop
               SM := Declaration_Subtype_Mark (Discrs (I));
               case Expression_Kind (SM) is
                  when A_Selected_Component =>
                     SM := Selector (SM);
                  when An_Attribute_Reference =>
                     -- Cannot be 'Class since it is discrete.
                     -- The kind of 'Base is the same as the one of the type itself.
                     SM := Prefix (SM);
                  when An_Identifier =>
                     null;
                  when others =>
                     Impossible ("Wrong declaration_subtype_mark", SM);
               end case;

               if Contains_Type_Declaration_Kind (Corresponding_Name_Declaration (SM), Kind) then
                  return True;
               end if;
            end loop;
         end;
         return False;
      end Discriminants_Contain_Type_Declaration_Kind;

      Decl : Asis.Declaration := The_Subtype;
      Def  : Asis.Definition;
   begin
      Decl := Corresponding_First_Subtype (Decl);
      Def  := Type_Declaration_View (Decl);
      if Type_Kind (Def) = A_Derived_Type_Definition then
         Decl := A4G_Bugs.Corresponding_Root_Type (Type_Declaration_View (Decl));
         Def  := Type_Declaration_View (Decl);
      end if;

      if Declaration_Kind (Decl) = Kind then
         return True;
      else
         case Declaration_Kind (Decl) is
            when An_Ordinary_Type_Declaration =>
               case Type_Kind (Def) is
                  when An_Unconstrained_Array_Definition | A_Constrained_Array_Definition =>
                     return Contains_Type_Declaration_Kind (Corresponding_Name_Declaration
                                                            (Subtype_Simple_Name
                                                             (Component_Subtype_Indication
                                                              (Array_Component_Definition (Def)))),
                                                            Kind);

                  when A_Record_Type_Definition |A_Tagged_Record_Type_Definition =>
                     if Definition_Kind (Asis.Definitions.Record_Definition (Def)) /= A_Null_Record_Definition
                       and then Components_Contain_Type_Declaration_Kind (Record_Components
                                                                          (Asis.Definitions.Record_Definition (Def)))
                     then
                        return True;
                     end if;
                     return Discriminants_Contain_Type_Declaration_Kind (Discriminant_Part (Decl));

                  when A_Derived_Record_Extension_Definition =>
                     if Contains_Type_Declaration_Kind (Corresponding_Name_Declaration
                                                        (Subtype_Simple_Name (Parent_Subtype_Indication (Def))),
                                                       Kind)
                     then
                        return True;
                     end if;
                     if Definition_Kind (Asis.Definitions.Record_Definition (Def)) /= A_Null_Record_Definition
                       and then Components_Contain_Type_Declaration_Kind (Record_Components
                                                                          (Asis.Definitions.Record_Definition (Def)))
                     then
                        return True;
                     end if;
                     return Discriminants_Contain_Type_Declaration_Kind (Discriminant_Part (Decl));

                  when others =>
                     return False;
               end case;


            when A_Task_Type_Declaration =>
               -- Tasks contain no components
               return Discriminants_Contain_Type_Declaration_Kind (Discriminant_Part (Decl));

            when A_Protected_Type_Declaration =>
               -- Only the private part can contain components
               declare
                  Decls : constant Asis.Declarative_Item_List := Private_Part_Items (Def);
               begin
                  for I in Decls'Range loop
                     if Declaration_Kind (Decls (I)) = A_Component_Declaration
                       and then Contains_Type_Declaration_Kind (Corresponding_Name_Declaration
                                                                (Subtype_Simple_Name
                                                                 (Component_Subtype_Indication
                                                                  (Object_Declaration_View (Decls (I))))),
                                                                Kind)
                     then
                        return True;
                     end if;
                  end loop;
               end;
               return Discriminants_Contain_Type_Declaration_Kind (Discriminant_Part (Decl));

            when A_Private_Extension_Declaration =>
               if Contains_Type_Declaration_Kind (Corresponding_Name_Declaration
                                                  (Subtype_Simple_Name (Ancestor_Subtype_Indication (Def))),
                                                 Kind)
               then
                  return True;
               end if;
               return Discriminants_Contain_Type_Declaration_Kind (Discriminant_Part (Decl));

            when others =>
               return False;
         end case;
      end if;
   end Contains_Type_Declaration_Kind;

   -------------------
   -- Profile_Image --
   -------------------

   function Profile_Image (The_Name : Asis.Element; With_Profile : Boolean := True)
                          return Wide_String
   is
      use Asis.Expressions;

      Decl_Name : Asis.Defining_Name;

      function Entry_Name (The_Entry : Profile_Entry) return Wide_String is
         function Add_Attribute (S : Wide_String) return Wide_String is
         begin
            case The_Entry.Attribute is
               when None =>
                  return S;
               when Base =>
                  return S & "'BASE";
               when Class =>
                  return S & "'CLASS";
            end case;
         end Add_Attribute;

      begin
         if The_Entry.Is_Access then
            return '*' & Add_Attribute (Full_Name_Image (The_Entry.Name, With_Profile));
         else
            return Add_Attribute (Full_Name_Image (The_Entry.Name, With_Profile));
         end if;
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

   begin
      if Element_Kind (The_Name) = A_Defining_Name then
         Decl_Name := The_Name;
      elsif Expression_Kind (The_Name) = A_Selected_Component then
         Decl_Name := Corresponding_Name_Definition (Selector (The_Name));
      else
         Decl_Name := Corresponding_Name_Definition (The_Name);
      end if;

      if Is_Callable_Construct (Decl_Name) then
         declare
            Profile_Names : constant Profile_Descriptor := Types_Profile (Enclosing_Element (Decl_Name));
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
      else
         return "";
      end if;
   end Profile_Image;

   -------------------
   -- Types_Profile --
   -------------------

   function Types_Profile (Declaration : in Asis.Declaration)  return Profile_Descriptor is
      use Asis.Expressions;

      function Build_Entry (Mark : Asis.Element) return Profile_Entry is
         -- To be honnest, builds the entry except the Is_Access field
         Good_Mark : Asis.Element;
         Attribute : Type_Attribute;
         Decl      : Asis.Declaration;
      begin
         if Expression_Kind (Mark) = An_Attribute_Reference then
            Good_Mark := Prefix (Mark);

            case A4G_Bugs.Attribute_Kind (Mark) is
               when A_Base_Attribute =>
                  Attribute := Base;
               when A_Class_Attribute =>
                  Attribute := Class;
               when others =>
                  -- Impossible
                  Impossible ("Attribute of Type_Profile = "
                                & Attribute_Kinds'Wide_Image (A4G_Bugs.Attribute_Kind (Mark)),
                              Declaration);
            end case;

         else
            Good_Mark := Mark;
            Attribute := None;
         end if;

         if Expression_Kind (Good_Mark) = A_Selected_Component then
            Good_Mark := Selector (Good_Mark);
         end if;

         Decl := Corresponding_Name_Declaration (Good_Mark);
         if Declaration_Kind (Decl) = An_Incomplete_Type_Declaration then
            -- cannot take the Corresponding_First_Subtype of an incomplete type
            Decl := Corresponding_Type_Declaration (Decl);
         end if;
         return (Is_Access => False,
                 Attribute => Attribute,
                 Name      => Names (Corresponding_First_Subtype (Decl))(1));
      end Build_Entry;

      function Build_Profile (Parameters : Parameter_Specification_List) return Profile_Table is
         -- Assert: parameters is not an empty list
         -- This function is written to avoid recursivity if there is no other multiple
         -- parameter declaration than the first one.

         Names_1    : constant Name_List := Names (Parameters (Parameters'First));
         Entry_1    : Profile_Entry := Build_Entry (Declaration_Subtype_Mark
                                                      (Parameters (Parameters'First)));
         Result     : Profile_Table (1 .. Names_1'Length + Parameters'Length -1);
         Result_Inx : Asis.List_Index;
      begin
         Entry_1.Is_Access := Trait_Kind (Parameters (Parameters'First)) = An_Access_Definition_Trait;
         for I in Natural range 1 .. Names_1'Length loop
            Result (I) := Entry_1;
         end loop;

         Result_Inx := Names_1'Length;
         for I in Natural range Parameters'First + 1 .. Parameters'Last loop
            declare
               Names_Rest : constant Name_List := Names (Parameters (I));
            begin
               if Names_Rest'Length = 1 then
                  Result_Inx := Result_Inx + 1;
                  Result (Result_Inx) := Build_Entry (Declaration_Subtype_Mark (Parameters (I)));
                  Result (Result_Inx).Is_Access := Trait_Kind (Parameters (I)) = An_Access_Definition_Trait;
               else
                  return Result (1 .. Result_Inx) &
                    Build_Profile (Parameters (I .. Parameters'Last));
               end if;
            end;
         end loop;
         return Result;
      end Build_Profile;

      Result_Entry     : Profile_Entry;
      Good_Declaration : Asis.Declaration := Declaration;
   begin  --  Types_Profile
      if Declaration_Kind (Good_Declaration) in A_Generic_Instantiation then
         -- We must get the profile from the corresponding generic element
         Good_Declaration := Corresponding_Declaration (Good_Declaration);
      end if;

      case Declaration_Kind (Good_Declaration) is
         when A_Function_Declaration |
           A_Function_Body_Declaration |
           A_Function_Renaming_Declaration |
           A_Function_Body_Stub |
           A_Generic_Function_Declaration |
           A_Formal_Function_Declaration =>

            Result_Entry := Build_Entry (Result_Profile (Good_Declaration));

         when An_Enumeration_Literal_Specification =>
            -- Profile for an enumeration litteral
            -- Like a parameterless function; go up two levels (type specification then type declaration)
            -- to find the return type.
            -- of the Enumaration_Literal_Specification
            -- Return immediately, since we know there are no parameters, and Parameter_Profile
            -- would choke on this.
            return (Formals_Length => 0,
                    Result_Type    => (Is_Access => False,
                                       Attribute => None,
                                       Name      => Names (Enclosing_Element
                                                             (Enclosing_Element
                                                                (Good_Declaration)))(1)),
                    Formals        => (others => (False, None, Nil_Element)));
         when others =>
            Result_Entry := (Is_Access => False,
                             Attribute => None,
                             Name      => Nil_Element);
      end case;

      declare
         Parameters : constant Asis.Parameter_Specification_List := Parameter_Profile (Good_Declaration);
      begin
         if Parameters'Length = 0 then
            return (Formals_Length => 0,
                    Result_Type    => Result_Entry,
                    Formals        => (others => (False, None, Nil_Element)));
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


   ------------------------------
   -- Ultimate_Expression_Type --
   ------------------------------

   function Ultimate_Expression_Type (The_Element : Asis.Expression) return Asis.Definition is
      use Asis.Expressions;

      Local_Elem : Asis.Element := A4G_Bugs.Corresponding_Expression_Type (The_Element);
      Def        : Asis.Definition;
   begin
      if Is_Nil (Local_Elem) then
         -- The_Element is a package, subprogram, task...
         -- For task and protected, we can still go to the definition
         case Expression_Kind (The_Element) is
            when A_Selected_Component =>
               Local_Elem := Selector (The_Element);
            when An_Identifier =>
               Local_Elem := The_Element;
            when others =>
               return Nil_Element;
         end case;

         Local_Elem := Corresponding_Name_Declaration (Local_Elem);
         case Declaration_Kind (Local_Elem) is
            when A_Variable_Declaration
              | A_Constant_Declaration
              | A_Deferred_Constant_Declaration
              =>
               Def := Object_Declaration_View (Local_Elem);
               if Definition_Kind (Def) = A_Type_Definition then
                  -- This can only be an anonymous array => we have the definition
                  return Def;
               else
                  return Type_Declaration_View (Corresponding_Name_Declaration
                                                (Subtype_Simple_Name
                                                 (Def)));
               end if;
            when A_Single_Protected_Declaration
              | A_Single_Task_Declaration
              =>
               return Object_Declaration_View (Local_Elem);
            when others =>
               return Nil_Element;
         end case;
      end if;

      -- Go to the full declaration if necessary (incomplete and private)
      if Declaration_Kind (Local_Elem) in
        An_Incomplete_Type_Declaration .. A_Private_Extension_Declaration
      then
         Local_Elem := Corresponding_Type_Declaration (Local_Elem);
      end if;

      Local_Elem := Type_Declaration_View (Corresponding_First_Subtype (Local_Elem));

      if Definition_Kind (Local_Elem) in
        A_Private_Type_Definition .. A_Private_Extension_Definition
      then
         Local_Elem := Type_Declaration_View (Corresponding_First_Subtype
                                                (Corresponding_Type_Declaration
                                                   (Enclosing_Element (Local_Elem))));
      end if;

      if Type_Kind (Local_Elem) in
        A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
      then
         Local_Elem := Type_Declaration_View (A4G_Bugs.Corresponding_Root_Type (Local_Elem));
      end if;

      return Local_Elem;
   end Ultimate_Expression_Type;

   -------------------
   -- Ultimate_Name --
   -------------------

   function Ultimate_Name (The_Name : Asis.Element) return Asis.Element is
      use Asis.Expressions;
      Decl   : Asis.Declaration;
      Result : Asis.Element := The_Name;
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
      elsif Expression_Kind (The_Name) = A_Selected_Component then
         Result := Selector (The_Name);
         Decl   := Corresponding_Name_Declaration (Result);
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
         Result := A4G_Bugs.Corresponding_Base_Entity (Decl);
         loop
            case Expression_Kind (Result) is
               when A_Selected_Component =>
                  Result := Selector (Result);
               when A_Slice
                 | An_Indexed_Component
                 =>
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

   --------------------------------------
   -- Ultimate_Enclosing_Instantiation --
   --------------------------------------

   function Ultimate_Enclosing_Instantiation (The_Element : Asis.Element) return Asis.Declaration is
      Result : Asis.Declaration := Enclosing_Element (The_Element);
   begin
      while (Declaration_Kind (Result) not in A_Generic_Instantiation
             and Declaration_Kind (Result) not in A_Formal_Package_Declaration .. A_Formal_Package_Declaration_With_Box)
        or else Is_Part_Of_Instance (Result)
      loop
         Result := Enclosing_Element (Result);
      end loop;

      return Result;
   end Ultimate_Enclosing_Instantiation;

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

      else
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
      end if;

      return Is_Part_Of_Generic (Enclosing_Element (Parent_Name));
   end Is_Part_Of_Generic;

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


   ----------------------------------
   -- Discrete_Constraining_Bounds --
   ----------------------------------

   function Discrete_Constraining_Bounds (Elem : Asis.Element) return Asis.Element_List is
      use Asis.Definitions, Asis.Expressions;

      function Constraint_Bounds (Def : Asis.Constraint) return Expression_List;

      function Range_Attribute_Bounds (Attr : Asis.Expression) return Asis.Expression_List is
         Bounds   : constant Asis.Expression_List := Discrete_Constraining_Bounds (Prefix (Attr));
         Dim_Expr : constant Asis.Expression_List := Attribute_Designator_Expressions (Attr);
         Dim      : Positive;
      begin
         if Is_Nil (Bounds) then
            -- 'Range of a generic formal type
            return Nil_Element_List;
         elsif Is_Nil (Dim_Expr) then
            Dim := 1;
         else
            -- In the extremely unlikely case where the static expression Dim_Expr is
            -- too complicated for us to evaluate, the following will raise Constraint_Error,
            -- and thus we will return "", which is appropriate.
            Dim := Integer'Wide_Value (Static_Expression_Value_Image (Dim_Expr (1)));
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
                  return Discrete_Constraining_Bounds (Subtype_Simple_Name (Def));
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
                     Result (2*I-1 .. 2*I) := Discrete_Range_Bounds (Index_Ranges (I));
                  end loop;
                  return Result;
               end;
            when A_Simple_Expression_Range =>
               return (Lower_Bound (Def), Upper_Bound (Def));
            when A_Range_Attribute_Reference =>
               return Range_Attribute_Bounds (Range_Attribute (Def));
        end case;
      end Constraint_Bounds;

      Item       : Asis.Element := Elem; -- This item will navigate until we find the appropriate definition
      Item_Def   : Asis.Definition;
      Parent     : Subtype_Indication;
      Constraint : Asis.Definition;
   begin  -- Discrete_Constraining_Bounds
      -- Get rid of cases managed by other functions
      case Definition_Kind (Elem) is
         when A_Constraint =>
            return Constraint_Bounds (Elem);
         when A_Discrete_Range
           | A_Discrete_Subtype_Definition
           =>
            return Discrete_Range_Bounds (Elem);
         when others =>
            null;
      end case;

      -- Find a good declaration
      loop
         if Element_Kind (Item) = An_Expression then
            -- A (possibly selected) name
            case Expression_Kind (Item) is
               when A_Selected_Component =>
                  Item := Corresponding_Name_Declaration (Selector (Item));
               when An_Attribute_Reference =>
                  -- We cannot get to the bounds of T'Base, and taking T instead would be misleading
                  -- There is no applicable constraints for 'Class
                  -- Other attributes are values or subprograms, not interesting for us.
                  -- Therefore, in all cases:
                  return Nil_Element_List;
               when An_Explicit_Dereference =>
                  -- We must go to the declaration of the type referenced by the prefix
                  Item := Type_Declaration_View (A4G_Bugs.Corresponding_Expression_Type (Prefix (Item)));
                  if Access_Type_Kind (Item) in An_Access_To_Procedure .. An_Access_To_Protected_Function then
                     return Nil_Element_List;
                  end if;
                  Item       := Asis.Definitions.Access_To_Object_Definition (Item);
                  Constraint := Subtype_Constraint (Item);
                  if Is_Nil (Constraint) then
                     Item :=  Corresponding_Name_Declaration (Subtype_Simple_Name (Item));
                  else
                     -- Constraint given in the subtype => no need to go further
                     return Constraint_Bounds (Constraint);
                  end if;
               when A_Function_Call =>
                  -- The constraint is the one of the return type
                  Item := A4G_Bugs.Corresponding_Expression_Type (Item);
               when others =>
                   Item := Corresponding_Name_Declaration (Item);
            end case;
         end if;

         -- Invariant:
         -- Here, Item is a declaration
         case Declaration_Kind (Item) is
            when An_Ordinary_Type_Declaration =>
               Item_Def := Type_Declaration_View (Item);
               case Type_Kind (Item_Def) is
                  when Not_A_Type_Definition =>
                     Impossible ("Discrete_Constraining_Bounds: not a definition", Item);

                  when A_Derived_Type_Definition =>
                     Parent     := Parent_Subtype_Indication (Item_Def);
                     Constraint := Subtype_Constraint (Parent);
                     if Is_Nil (Constraint) then
                        Item := Subtype_Simple_Name (Parent);
                     else
                        -- Constraint given in the derivation => no need to go further
                        return Constraint_Bounds (Constraint);
                     end if;

                  when A_Derived_Record_Extension_Definition
                    | A_Record_Type_Definition
                    | A_Tagged_Record_Type_Definition
                    | An_Access_Type_Definition
                    =>
                     -- Not a discrete type
                     return Nil_Element_List;

                  when An_Enumeration_Type_Definition =>
                     declare
                        Literals : constant Asis.Declaration_List := Enumeration_Literal_Declarations (Item_Def);
                     begin
                        return (Names (Literals (Literals'First))(1), Names (Literals (Literals'Last))(1));
                     end;

                  when A_Modular_Type_Definition =>
                     return (Nil_Element, Mod_Static_Expression (Item_Def));

                  when A_Signed_Integer_Type_Definition =>
                     declare
                        Constr : constant Asis.Constraint := Integer_Constraint (Item_Def);
                     begin
                        return (Lower_Bound (Constr), Upper_Bound (Constr));
                     end;

                  when A_Root_Type_Definition =>
                     case Root_Type_Kind (Item_Def) is
                        when A_Root_Integer_Definition
                          | A_Universal_Integer_Definition
                          =>
                           -- Bounds of Root_Integer are System.Min_Int .. System.Max_Int, 3.5.4(14)
                           -- However, we have no (easy) way to retrieve them as Element
                           -- Bounds of Universal_Integer are infinite.
                           -- Just pretend they are both not computable
                           return Nil_Element_List;
                        when A_Root_Real_Definition
                          | A_Universal_Real_Definition
                          | A_Universal_Fixed_Definition
                          =>
                           -- Only discrete bounds for the moment
                           return Nil_Element_List; -- TBSL
                        when Not_A_Root_Type_Definition =>
                           Impossible ("Wrong root type in Discrete_Constraining_Bounds", Item);
                     end case;

                  when A_Floating_Point_Definition
                    | An_Ordinary_Fixed_Point_Definition
                    | A_Decimal_Fixed_Point_Definition
                    =>
                     -- Only discrete bounds for the moment
                     return Nil_Element_List; -- TBSL

                  when An_Unconstrained_Array_Definition =>
                     declare
                        Index_Defs : constant Asis.Definition_List := Index_Subtype_Definitions (Item_Def);
                        Result     : Asis.Expression_List (1 .. 2*Index_Defs'Length);
                     begin
                        -- Index_Defs can only contain A_Discrete_Subtype_Indication here
                        for I in Index_Defs'Range loop
                          Result (2*I-1 .. 2*I) := Discrete_Constraining_Bounds (Index_Defs (I));
                        end loop;

                        return Result;
                     end;

                  when A_Constrained_Array_Definition =>
                     declare
                        Index_Defs : constant Asis.Definition_List := Discrete_Subtype_Definitions (Item_Def);
                        Result     : Asis.Expression_List (1 .. 2*Index_Defs'Length);
                     begin
                        for I in Index_Defs'Range loop
                           Result (2*I-1 .. 2*I) := Discrete_Range_Bounds (Index_Defs (I));
                        end loop;

                        return Result;
                     end;

                  when others =>  -- Compatibility Ada 2005
                     -- TBSL what is it?
                     return Nil_Element_List;
               end case;

            when A_Subtype_Declaration =>
               Item_Def   := Type_Declaration_View (Item);
               Constraint := Subtype_Constraint (Item_Def);
               if Is_Nil (Constraint) then
                  Item := Subtype_Simple_Name (Item_Def);
               else
                  -- Constraint given in the subtype => no need to go further
                  return Constraint_Bounds (Constraint);
               end if;

            when A_Task_Type_Declaration
              | A_Protected_Type_Declaration
              | A_Private_Extension_Declaration
              =>
               return Nil_Element_List;

            when An_Incomplete_Type_Declaration
              | A_Private_Type_Declaration
              =>
               Item := Corresponding_Type_Declaration (Item);

            when A_Variable_Declaration
              | A_Constant_Declaration
              | A_Component_Declaration
              =>
               Item_Def := Object_Declaration_View (Item);
               if Definition_Kind (Item_Def) = A_Component_Definition then
                 Item_Def := Component_Subtype_Indication (Item_Def);
               end if;

               case Definition_Kind (Item_Def) is
                  when A_Type_Definition =>
                     -- A_Constrained_Array_Definition
                     declare
                        Index_Defs : constant Asis.Definition_List := Discrete_Subtype_Definitions (Item_Def);
                        Result     : Asis.Expression_List (1 .. 2*Index_Defs'Length);
                     begin
                        for I in Index_Defs'Range loop
                           Result (2*I-1 .. 2*I) := Discrete_Range_Bounds (Index_Defs (I));
                        end loop;

                        return Result;
                     end;
                  when A_Task_Definition
                    | A_Protected_Definition
                    =>
                     -- Not a discrete type
                     return Nil_Element_List;
                  when A_Component_Definition =>
                     Impossible ("Discrete_Constraining_Bounds: Component definition", Item_Def);
                  when A_Subtype_Indication =>
                     Constraint := Subtype_Constraint (Item_Def);
                     if Is_Nil (Constraint) then
                        Item := Subtype_Simple_Name (Item_Def);
                     else
                        return Constraint_Bounds (Constraint);
                     end if;
                  when others =>
                     Impossible ("Discrete_Constraining_Bounds: Bad definition from object", Item_Def);
               end case;

            when A_Parameter_Specification
              | A_Formal_Object_Declaration
              =>
               -- No constraint allowed here, get bounds from the type
               Item := Declaration_Subtype_Mark (Item);

            when A_Formal_Type_Declaration =>
               -- TBSL
               -- Consider it non static for the moment
               -- If it is a formal array type, its bounds could be evaluable
               return Nil_Element_List;

            when others =>
               Impossible ("Discrete_Constraining_Bounds: Bad declaration", Elem);
         end case;
      end loop;
   end Discrete_Constraining_Bounds;


   -----------------------------------
   -- Discrete_Constraining_Lengths --
   -----------------------------------

   function Discrete_Constraining_Lengths (Elem : Asis.Element) return Extended_Biggest_Natural_List is
      Bounds : constant Asis.Element_List := Discrete_Constraining_Bounds (Elem);
      Result : Extended_Biggest_Natural_List (1 .. Bounds'Length / 2);
   begin
      -- TBSL return empty list for arrays?
      if Result'Length = 0 then
         return (Non_Static, Non_Static);
      end if;

      for I in Result'Range loop
         begin
            case Element_Kind (Bounds (2*I-1)) is
               when An_Expression =>
                  declare
                     Low  : constant Wide_String := Static_Expression_Value_Image (Bounds (2*I-1));
                     High : constant Wide_String := Static_Expression_Value_Image (Bounds (2*I));
                  begin
                     if Low = "" or High = "" then
                        -- Some bound is dynamic
                        Result (I) := Non_Static;
                     elsif Biggest_Int'Wide_Value (Low) > Biggest_Int'Wide_Value (High) then
                        Result (I) := 0;
                     else
                        Result (I) := Biggest_Int'Wide_Value (High) - Biggest_Int'Wide_Value (Low) + 1;
                     end if;
                  end;
               when A_Defining_Name =>
                  -- Enumeration
                  Result (I) := Biggest_Int'Wide_Value (Position_Number_Image (Bounds (2*I)))
                    - Biggest_Int'Wide_Value (Position_Number_Image (Bounds (2*I-1)))
                    + 1;
               when Not_An_Element =>
                  -- Modular type (not subtype)
                  Result (I) := Biggest_Int'Wide_Value (Static_Expression_Value_Image (Bounds (2*I)));
               when others =>
                  Impossible ("Bad return from Discrete_Range_Bounds", Bounds (2));
            end case;
         exception
            when Constraint_Error =>
               -- Not in range of Biggest_Int...
               Result (I) := Non_Static;
         end;
      end loop;

      return Result;
   end Discrete_Constraining_Lengths;


   ----------------
   -- Statements --
   ----------------

   function Statements (Element : in Asis.Element) return Asis.Statement_List is
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
                  return Body_Statements (Element);
               when others =>
                  Impossible ("Statements: invalid declaration kind", Element);
            end case;
         when A_Statement =>
            case Statement_Kind (Element) is
               when An_Accept_Statement =>
                  return Accept_Body_Statements (Element);
               when A_Block_Statement =>
                  return Block_Statements (Element);
               when others =>
                  Impossible ("Statements: invalid statement kind", Element);
            end case;
         when others =>
            Impossible ("Statements: invalid element kind", Element);
      end case;
   end Statements;

   -----------------------------------
   -- Static_Expression_Value_Image --
   -----------------------------------

   function Static_Expression_Value_Image (Expression : Asis.Expression) return Wide_String is
      use Asis.Expressions;

      generic
         with function Op (Left, Right : Biggest_Int) return Biggest_Int;
      function String_Op (Left, Right : Wide_String) return Wide_String;
      function String_Op (Left, Right : Wide_String) return Wide_String is
      begin
         if Left = "" or Right = "" then
            return "";
         end if;
         return Biggest_Int'Wide_Image (Op (Biggest_Int'Wide_Value (Left), Biggest_Int'Wide_Value (Right)));
      end String_Op;

      function "+" is new String_Op ("+");
      function "-" is new String_Op ("-");
      function "*" is new String_Op ("*");
      function "/" is new String_Op ("/");
      -- Cannot do the same for "**", since Right is always Natural
      function "**" (Left, Right : Wide_String) return Wide_String is
      begin
         if Left = "" or Right = "" then
            return "";
         end if;
         return Biggest_Int'Wide_Image ("**" (Biggest_Int'Wide_Value (Left), Natural'Wide_Value (Right)));
      end "**";

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
   begin
      case Expression_Kind (Expression) is
         when An_Integer_Literal =>
            -- We make a round-trip through Value/Image below to normalize the form of the result
            -- (get rid of based numbers and '_')
            return Biggest_Int'Wide_Image (Biggest_Int'Wide_Value (Value_Image (Expression)));

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
               Op_Name : Asis.Expression := Prefix (Expression);
            begin
               if Expression_Kind (Op_Name) = A_Selected_Component then
                  Op_Name := Selector (Op_Name);
               end if;

               case Expression_Kind (Op_Name) is
                  when An_Operator_Symbol =>
                     -- Check that the operator is the real one, not some user-defined function
                     -- For predefined operations, either there is no "fake" declaration and
                     -- Corresponding_Name_Declaration returns Nil_Element (GNAT case), or the
                     -- Declaration_Origin is An_Implicit_Predefined_Declaration.
                     Decl := A4G_Bugs.Corresponding_Called_Function (Expression);
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

                        when others =>
                           -- Not implemented, or Not_An_Operator
                           return "";
                     end case;

                  when An_Attribute_Reference =>
                     case A4G_Bugs.Attribute_Kind (Prefix (Expression)) is
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
            case A4G_Bugs.Attribute_Kind (Expression) is
               when A_First_Attribute =>
                  declare
                     Bounds   : constant Asis.Expression_List := Discrete_Constraining_Bounds (Prefix (Expression));
                     Dim_Expr : constant Asis.Expression_List := Attribute_Designator_Expressions (Expression);
                     Dim : Positive;
                  begin
                     if Is_Nil (Dim_Expr) then
                        Dim := 1;
                     else
                        -- In the extremely unlikely case where the static expression Dim_Expr is
                        -- too complicated for us to evaluate, the following will raise Constraint_Error,
                        -- and thus we will return "", which is appropriate.
                        Dim := Integer'Wide_Value (Static_Expression_Value_Image (Dim_Expr (1)));
                     end if;

                     case Element_Kind (Bounds (2*Dim-1)) is
                        when An_Expression =>
                           return Static_Expression_Value_Image (Bounds (2*Dim-1));
                        when A_Defining_Name =>
                           -- Enumeration
                           return Position_Number_Image (Bounds (2*Dim-1));
                        when Not_An_Element =>
                           -- Modular type
                           return " 0";
                        when others =>
                           Impossible ("Bad return from Discrete_Constraining_Bounds", Bounds (2*Dim-1));
                     end case;
                  end;
               when A_Last_Attribute =>
                  declare
                     Bounds   : constant Asis.Expression_List := Discrete_Constraining_Bounds (Prefix (Expression));
                     Dim_Expr : constant Asis.Expression_List := Attribute_Designator_Expressions (Expression);
                     Dim : Positive;
                  begin
                     if Is_Nil (Dim_Expr) then
                        Dim := 1;
                     else
                        -- In the extremely unlikely case where the static expression Dim_Expr is
                        -- too complicated for us to evaluate, the following will raise Constraint_Error,
                        -- and thus we will return "", which is appropriate.
                        Dim := Integer'Wide_Value (Static_Expression_Value_Image (Dim_Expr (1)));
                     end if;

                     case Element_Kind (Bounds (2*Dim-1)) is
                        when An_Expression =>
                           return Static_Expression_Value_Image (Bounds (2*Dim));
                        when A_Defining_Name =>
                           -- Enumeration
                           return Position_Number_Image (Bounds (2*Dim));
                        when Not_An_Element =>
                           -- Modular type
                           return Static_Expression_Value_Image (Bounds (2*Dim)) - "1";
                        when others =>
                           Impossible ("Bad return from Discrete_Constraining_Bounds", Bounds (2*Dim-1));
                     end case;
                  end;
               when others =>
                  return "";
            end case;

         when others =>
            -- Including Not_An_Expression
            return "";
      end case;

   exception
      when Constraint_Error =>
         -- Out of range of Biggest_Int f.e., give up
         return "";
   end Static_Expression_Value_Image;


   -------------------------
   -- Variables_Proximity --
   -------------------------

   function Variables_Proximity (Left, Right : Asis.Element) return Proximity is
      use Asis.Expressions;

      type Part_Kind is (Identifier, Field, Indexing, Dereference, Call, Not_Variable);
      type Name_Part (Kind : Part_Kind := Identifier) is
         record
            case Kind is
               when Identifier =>
                  Id_Name : Asis.Expression;
               when Field =>
                  Sel_Name : Asis.Expression;
               when Indexing =>
                  Indexers : Asis.Expression;
               when Dereference =>
                  null;
               when Call =>
                  null;
               when Not_Variable =>
                  null;
            end case;
         end record;

      type Name_Descriptor is array (Positive range <>) of Name_Part;

      function Descriptor (Name : Asis.Element; With_Deref : Boolean := False) return Name_Descriptor is
         E                  : Asis.Element := Name;
         Variable           : Asis.Definition;
         Variable_Enclosing : Asis.Element;

         function Complete_For_Access (D : Name_Descriptor) return Name_Descriptor is
            -- Add a "Dereference" part if the *type* is an access type
            -- This allows explicit and implicit dereferences to match
         begin
            if With_Deref and Expression_Type_Kind (Name) = An_Access_Type_Definition then
               return D & Name_Part'(Kind => Dereference);
            else
               return D;
            end if;
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
                  Variable := Corresponding_Name_Definition (E);
                  Variable_Enclosing := Enclosing_Element (Variable);
                  if Declaration_Kind (Variable_Enclosing) in A_Renaming_Declaration then
                     E := Renamed_Entity (Variable_Enclosing);
                  else
                     return Complete_For_Access ((1 => (Identifier, Variable)));
                  end if;

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
                        return (1 => (Kind => Not_Variable));
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
                  return Complete_For_Access ((1 => (Kind => Call)));

               when An_Explicit_Dereference =>
                   return Complete_For_Access (Descriptor (Prefix (E), With_Deref => True));

               when A_Type_Conversion =>
                  E := Converted_Or_Qualified_Expression (E);

               when others =>
                  return (1 => (Kind => Not_Variable));
            end case;
         end loop;
      end Descriptor;

      function Descriptors_Proximity (L_Descr, R_Descr : Name_Descriptor) return Proximity is
         -- Note that L_Descr'First and R_Descr'First are always 1
         Best_Conf : Result_Confidence;
         L_Rightmost_Deref : Natural := 1;
         R_Rightmost_Deref : Natural := 1;
         L_Inx, R_Inx : Positive;
         Base_Proximity : Proximity;
      begin

         -- First, compare the "base" variable and eliminate expressions that are not variables
         for I in reverse L_Descr'Range loop
            case L_Descr (I).Kind is
               when Dereference =>
                  L_Rightmost_Deref := I;
                  exit;
               when Not_Variable =>
                  return (Certain, None);
               when others =>
                  null;
            end case;
         end loop;
         for I in reverse R_Descr'Range loop
            case R_Descr (I).Kind is
               when Dereference =>
                  R_Rightmost_Deref := I;
                  exit;
               when Not_Variable =>
                  return (Certain, None);
               when others =>
                  null;
            end case;
         end loop;

         if L_Rightmost_Deref = 1 and R_Rightmost_Deref = 1 then
            -- No dereference on either side
            if (L_Descr (1).Kind = Identifier and R_Descr (1).Kind = Identifier)
              and then not Is_Equal (L_Descr (1).Id_Name, R_Descr (1).Id_Name)
            then
               return (Confidence => Certain, Overlap => None);
            elsif L_Descr (1).Kind = Call or R_Descr (1).Kind = Call then
               Best_Conf := Possible;
            else
               Best_Conf := Certain;
            end if;
         elsif L_Rightmost_Deref /= 1 and R_Rightmost_Deref /= 1 then
            -- Dereferences on both sides
            Base_Proximity := Descriptors_Proximity (L_Descr (1 .. L_Rightmost_Deref - 1),
                                                     R_Descr (1 .. R_Rightmost_Deref - 1));
            if Base_Proximity.Overlap = Complete then
               Best_Conf := Base_Proximity.Confidence;
            else
               Best_Conf := Unlikely;
            end if;
         else
            Best_Conf := Unlikely;
         end if;

         -- Compare the rest
         L_Inx := L_Rightmost_Deref + 1;
         R_Inx := R_Rightmost_Deref + 1;
         while L_Inx <= L_Descr'Last and R_Inx <= R_Descr'Last loop
            if L_Descr (L_Inx).Kind /= R_Descr (R_Inx).Kind then
               return (Confidence => Certain, Overlap => None);
            end if;
            case L_Descr (L_Inx).Kind is
               when Identifier
                 | Not_Variable
                 =>
                  Impossible ("Variables proximity: " & Part_Kind'Wide_Image (L_Descr (L_Inx).Kind),
                              L_Descr (L_Inx).Id_Name);
               when Field =>
                  if not Is_Equal (Corresponding_Name_Definition (L_Descr (L_Inx).Sel_Name),
                                   Corresponding_Name_Definition (R_Descr (R_Inx).Sel_Name))
                  then
                     return (Confidence => Certain, Overlap => None);
                  end if;
               when Indexing =>
                  declare
                     L_Indexes : constant Asis.Expression_List := Index_Expressions (L_Descr (L_Inx).Indexers);
                     R_Indexes : constant Asis.Expression_List := Index_Expressions (R_Descr (R_Inx).Indexers);
                  begin
                     if L_Indexes'Length /= R_Indexes'Length then
                        Impossible ("Variables proximity: different lengthes", Nil_Element);
                     end if;
                     for I in L_Indexes'Range loop
                        declare
                           L_Value : constant Wide_String := Static_Expression_Value_Image (L_Indexes (I));
                           R_Value : constant Wide_String := Static_Expression_Value_Image (R_Indexes (I));
                        begin
                           if L_Value = "" or R_Value = "" then
                              Best_Conf := Result_Confidence'Min (Best_Conf, Possible);
                           elsif L_Value /= R_Value then
                              return (Confidence => Certain, Overlap => None);
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
      use Asis.Expressions;

      Good_Left, Good_Right : Asis.Expression;
      Decl : Asis.Declaration;
   begin
      if Expression_Kind (Left) = A_Selected_Component then
         Good_Left := Selector (Left);
      else
         Good_Left := Left;
      end if;
      if Expression_Kind (Right) = A_Selected_Component then
         Good_Right := Selector (Right);
      else
         Good_Right := Right;
      end if;

      if Expression_Kind (Good_Left) = An_Identifier and Expression_Kind (Good_Right) = An_Identifier then
         Good_Left  := Ultimate_Name (Good_Left);
         Good_Right := Ultimate_Name (Good_Right);
         Decl       := Corresponding_Name_Declaration (Good_Left);
         case Declaration_Kind (Decl) is
            when A_Constant_Declaration =>
               if Is_Equal (Corresponding_Name_Definition (Good_Left),
                            Corresponding_Name_Definition (Good_Right))
               then
                  return True;
               end if;
            when A_Parameter_Specification =>
               if Mode_Kind (Decl) in A_Default_In_Mode .. An_In_Mode then
                  if Is_Equal (Corresponding_Name_Definition (Good_Left),
                               Corresponding_Name_Definition (Good_Right))
                  then
                     return True;
                  end if;
               end if;
            when others =>
               null;
         end case;
      end if;

      -- Here, Left and Right do not refer to the same constant or in parameter
      declare
         Left_Value : constant Wide_String := Static_Expression_Value_Image (Left);
         Right_Value :constant Wide_String := Static_Expression_Value_Image (Right);
      begin
         return Left_Value /= "" and then Left_Value = Right_Value;
      end;
   end Same_Value;

end Thick_Queries;

