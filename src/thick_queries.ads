----------------------------------------------------------------------
--  Thick_Queries - Package specification                           --
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

with Asis;
with System;
package Thick_Queries is

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Error report                                                                                --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   type Error_Procedure is access procedure (Message : Wide_String; Elem : Asis.Element);
   procedure Set_Error_Procedure (To : Error_Procedure);
   -- Defines a user defined procedure called in case of an internal error
   -- ("impossible" cases, or calls with inappropriate elements)
   -- The user procedure may raise an exception. If it doesn't (or no user procedure is defined),
   -- Program_Error will be raised.

   procedure Report_Error (Message : Wide_String; E : Asis.Element := Asis.Nil_Element);
   pragma No_Return (Report_Error);

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Convenience subtypes (for binary operators only)                                            --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------
   subtype Logical_Operators     is Asis.Operator_Kinds range Asis.An_And_Operator     .. Asis.An_Xor_Operator;
   subtype Equality_Operators    is Asis.Operator_Kinds range Asis.An_Equal_Operator   .. Asis.A_Not_Equal_Operator;
   subtype Relational_Operators  is Asis.Operator_Kinds range Asis.An_Equal_Operator   ..
                                                                              Asis.A_Greater_Than_Or_Equal_Operator;
   subtype Adding_Operators      is Asis.Operator_Kinds range Asis.A_Plus_Operator     .. Asis.A_Minus_Operator;
   subtype Multiplying_Operators is Asis.Operator_Kinds range Asis.A_Multiply_Operator .. Asis.A_Rem_Operator;

   subtype Discrete_Type_Kinds is Asis.Type_Kinds
   range Asis.An_Enumeration_Type_Definition .. Asis.A_Modular_Type_Definition;
   subtype Fixed_Type_Kinds    is Asis.Type_Kinds
      range Asis.An_Ordinary_Fixed_Point_Definition .. Asis.A_Decimal_Fixed_Point_Definition;

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   --  General types                                                                              --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   type Privacy_Policy is (Follow_Private, Follow_User_Private, Stop_At_Private);
   -- Used to define the behaviour of some queries when they encounter a private type
   -- - Follow_Private: Don't stop at private type, use the properties of the full type (privacy breaking)
   -- - Stop_At_Private: Stop at private types, don't consider full types (non privacy breaking)
   -- - Follow_User_Private: Like Follow_Private for user-defined private types, and like
   --                        Stop_At_Private for language defined and implementation defined private types

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about program structure                                                             --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Enclosing_Program_Unit (Element          : Asis.Element;
                                    Including_Accept : Boolean      := False)
                                   return Asis.Defining_Name;
   --  Return the Defining_Name of the innermost enclosing program unit of any Element
   --  If Including_Accept is true and the element is within an accept statement, return
   --  the corresponding entry name from the entry declaration (not really a program unit,
   --  but useful f.e. if the Element is a Return_Statement, and you want to know what you
   --  are returning from).
   --
   --  Appropriate Element_Kinds:
   --     Any element
   --
   --  Returns
   --     A_Defining_Name
   --     Nil_Element if Element is Nil, or is a Compilation_Unit
   --

   function Is_Compilation_Unit (Element : Asis.Element) return Boolean;
   --  Return True if Element is the declaration of its enclosing compilation unit
   --
   --  Appropriate Element_Kinds:
   --     Any element

   function Is_Ancestor (Outer : Asis.Compilation_Unit; Inner : Asis.Compilation_Unit; Strict : in Boolean := False)
                         return Boolean;
   -- Returns True if Inner Is_Equal to Outer (if Strict=False) or one of its (direct or indirect) children

   Global_Level : constant Asis.ASIS_Natural := 0;
   function Static_Level (Element : Asis.Element) return Asis.ASIS_Natural;
   -- Returns the static scope nesting level of Element
   -- Library units and entities declared in library packages have level 0 (global)
   -- Subprograms and tasks add 1 to the level (not packages)
   --  Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (checks the selector)

   function First_Enclosing_Instantiation (The_Element : Asis.Element) return Asis.Declaration;
   -- For an entity which Is_Part_Of_Instance:
   -- Return the first enclosing instantiation, which can itself be Is_Part_Of_Instance

   function Ultimate_Enclosing_Instantiation (The_Element : Asis.Element) return Asis.Declaration;
   -- For an entity which Is_Part_Of_Instance:
   -- Return the "true" instantiation, i.e. the one written by the user, going up instantiations
   -- that appear in generics.

   function Is_Generic_Unit (Element : in Asis.Element) return Boolean;
   -- Returns True if Element is the declaration or the body of a generic unit
   -- Returns False in all other cases
   --
   --  Appropriate Element_Kinds:
   --     Any element


   function Is_Part_Of_Generic (Element : in Asis.Element) return Boolean;
   -- Checks whether the Element is included (directly or indirectly) in a generic
   --
   --  Appropriate Element_Kinds:
   --     Any element

   function Ultimate_Origin (Element : in Asis.Element) return Asis.Unit_Origins;
   -- Returns the Unit_Origin of the Unit where Element is declared.
   -- If Element is an instance or Is_Part_Of_Instance, returns the Unit_Origin of the
   -- corresponding generic or generic element.
   -- Follows renamings to avoid being fooled by (user) renaming of Standard units.
   --
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (checks the selector)

   function Definition_Compilation_Unit (Element : in Asis.Element) return Asis.Compilation_Unit;
   -- Returns the compilation unit in which Element is declared
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (checks the selector)

   function Corresponding_Static_Exception_Handler (Exc            : Asis.Element;
                                                    Where          : Asis.Element;
                                                    Include_Others : Boolean)
                                                    return Asis.Exception_Handler;
   -- Returns the innermost exception handler that handles the given exception, and whose corresponding
   -- sequence of statements statically encloses Where.
   -- Returns a Nil_Element if no such handler is found
   --
   -- If the handler is a "when others" one, return it if Include_Others is true,
   -- return Nil_Element otherwise.
   --
   -- Look up stops as soon as a callable entity (or task body) is encountered (since from that point on,
   -- exception propagation is no more statically determinable).
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (checks the selector)


   function Is_Handled_Sequence_Container (Element : in Asis.Element) return Boolean;
   -- Returns True if Element is one of the elements that can contain a handled sequence of statements
   -- Returns false otherwise

   function Declarative_Items (Element : in Asis.Element; Include_Pragmas : in Boolean := False)
                              return Asis.Declaration_List;
   -- Returns the declarative items of any construct with declarative items.
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Statement
   --
   -- Appropriate Declaration_Kinds:
   --    A_Package_Declaration
   --    A_Generic_Package_Declaration
   --    A_Function_Body_Declaration
   --    A_Procedure_Body_Declaration
   --    A_Package_Body_Declaration
   --    A_Task_Body_Declaration
   --    An_Entry_Body_Declaration
   --    A_Protected_Body_Declaration
   --
   -- Appropriate Statement_Kinds:
   --    A_Block_Statement


   function Statements (Element         : in Asis.Element;
                        Include_Pragmas : in Boolean := False) return Asis.Statement_List;
   -- Returns the statements of any construct with statements.
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Statement
   --    A_Path
   --    An_Exception_Handler
   --
   -- Appropriate Declaration_Kinds:
   --    A_Function_Body_Declaration
   --    A_Procedure_Body_Declaration
   --    A_Package_Body_Declaration
   --    A_Task_Body_Declaration
   --    An_Entry_Body_Declaration
   --
   -- Appropriate Statement_Kinds:
   --    An_Accept_Statement
   --    A_Block_Statement
   --    A_Loop_Statement
   --    A_While_Loop_Statement
   --    A_For_Loop_Statement
   --    An_Extended_Return_Statement

   function Exception_Handlers (Element : Asis.Element) return Asis.Exception_Handler_List;
   -- Returns the exception handlers of any construct with exception hanlders.
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Statement
   --
   -- Appropriate Declaration_Kinds:
   --    A_Function_Body_Declaration
   --    A_Procedure_Body_Declaration
   --    A_Package_Body_Declaration
   --    A_Task_Body_Declaration
   --    An_Entry_Body_Declaration
   --
   -- Appropriate Statement_Kinds:
   --    An_Accept_Statement
   --    A_Block_Statement
   --    An_Extended_Return_Statement


   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about statements                                                                    --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Last_Effective_Statement (Stats : Asis.Statement_List) return Asis.Statement;
   -- Returns the last statement from Stats, unless it is a block statement without exception handlers,
   -- in which case the last statement of the block is returned (recursively, of course).

   function Are_Null_Statements (Stats : Asis.Statement_List; Except_Labelled : Boolean := False) return Boolean;
   -- Checks whether Stats contain only null statement(s)
   -- If Except_Labelled is True, returns False also if there is a labelled statement

   function First_Exiting_Statement (Stats : Asis.Statement_List; Include_Returns : Boolean := True)
                                     return Asis.Statement;
   -- Returns the (textually) first exit or goto statement that transfer control outside of Stats.
   -- If Include_Returns is True, also possibly returns a return statement, an extended return statement,
   -- or a requeue statement.

   function Is_Part_Of (Elem : Asis.Element; Inside : Asis.Element)      return Boolean;
   function Is_Part_Of (Elem : Asis.Element; Inside : Asis.Element_List) return Boolean;
   -- returns true if Elem is textually within Inside (i.e.  for a list, between the beginning of the first element
   -- and the end of the last element of Inside).
   -- Note: will return False for any element that Is_Part_Of_Instance, since these have no
   --       *textual* representation. The caller should take care of calling Corresponding_Generic_Element
   --       as needed to recognize elements that are Is_Part_Of_Instance

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Images                                                                                      --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Element_List_Image (List : Asis.Element_List; Separator : Wide_Character) return Wide_String;
   -- Like Element_Image, but for an element list
   -- Concatenates elements images, separated by Separator


   function Attribute_Name_Image (Attribute : Asis.Expression) return Wide_String;
   --  Like Pragma_Name_Image, but for an Attribute_Reference
   --
   --  Appropriate Element_Kinds:
   --    An_Expression
   --  Appropriate Expression_Kinds:
   --    An_Attribute_Reference


   function Extended_Name_Image (Name_Elem               : Asis.Element;
                                 Silent_If_Inappropriate : Boolean := False) return Wide_String;
   -- Image of a Name, Defining_Name, or pragma given either as a simple name or as a Selected_Name
   -- If Silent_If_Inappropriate is True, returns "" for inappropriate elements instead of raising an exception
   --
   --  Appropriate Element_Kinds:
   --    An_Expression
   --    A_Defining_Name
   --    A_Pragma
   --  Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component
   --    An_Attribute_Reference
   --    An_Explicit_Dereference


   function Full_Name_Image (The_Name     : in Asis.Element;
                             With_Profile : in Boolean := False) return Wide_String;
   -- Full name of a name
   -- Works like Asis.Declarations.Defining_Name_Image (or Name_Image),
   -- but returns the full (unique) name of The_Name, starting from the
   -- enclosing compilation unit (Standard for predefined elements).
   -- If With_Profile is true, "mangles" the name with a profile to provide a name
   -- that is unique even if overloaded.
   --
   -- Returns "" if the Full_Name_Image cannot be computed. This happens:
   --   - if The_Name is a predefined operator and With_Profile is True.
   --   - if The_Name is a subprogram name or formal parameter from a dispatching call
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (returns the image of the selector)
   --    An_Attribute_Reference


   function Profile_Image (The_Name     : Asis.Element;
                           With_Profile : Boolean := True) return Wide_String;
   -- Image of the profile of a callable construct
   -- If name is not a callable construct, returns ""
   -- Otherwise:
   --    for a procedure, entry...:
   --       returns '{' {<Full_Name_Image (types of parameter) '}'}
   --    for a function:
   --       returns '{' {<Full_Name_Image (types of parameter) } ':' Full_Name_Image (result type) '}'
   -- With_Profile determines if the Full_Name_Image generated for parameters and result includes
   -- itself a profile.
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (returns the image of the selector)


   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about types                                                                         --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Subtype_Simple_Name (Definition : Asis.Definition) return Asis.Expression;
   -- Like Subtype_Mark, but returns the selector if the subtype mark is a selected component
   -- Moreover, it avoids the ambiguity between Asis.Subtype_Mark and Asis.Definitions.Subtype_Mark

   function First_Subtype_Name (The_Subtype : Asis.Element) return Asis.Expression;
   -- Unwinds subtype declarations and returns the *name* of the first subtype denoted by The_Subtype,
   -- picked up from the last subtype declaration.
   -- Returns its argument if The_Subtype already denotes a first subtype.
   -- Unlike Corresponding_First_Subtype, this works in case of subtyping of a class wide type
   -- (returns the XXX'class)
   --
   -- Appropriate Element_Kinds
   --   A_Defining_Name
   --   An_Expression
   -- Appropriate Expression_Kinds:
   --   An_Identifier
   --   A_Selected_Component (works on selector)
   --   An_Attribute_Reference (T'Base only)
   --
   -- Returns Expression_Kinds:
   --   An_Identifier
   --   An_Attribute_Reference

   function Access_Target_Type (The_Subtype : Asis.Element) return Asis.Declaration;
   -- Returns the declaration of the first subtype of the target of the access type if The_Subtype
   -- is a declaration of an access to object type (including anonymous ones) or of a formal access
   -- to object type.
   -- It's the really first subtype ;-), ignoring 'Base and 'Class attributes that can be in the way.
   -- Returns Nil_Element in all other cases
   --
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name

   function Is_Access_Subtype (The_Subtype : Asis.Element) return Boolean;
   -- Returns True if The_Subtype is a declaration of an access type or of a formal access type
   -- or an access definition
   -- Returns False in all other cases
   --
   -- Appropriate Element_Kinds:
   --    A_Declaration (of a (sub)type)
   --    A_Definition  (of a (sub)type, or an access definition)
   --    A_Defining_Name (of a (sub)type)

   function Is_Array_Subtype (The_Subtype : Asis.Element) return Boolean;
   -- Returns True if The_Subtype is a declaration (or a definition) of an array type, or a name of an array type
   -- Returns False in all other cases
   --
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name
   --    An_Expression
   --
   -- Appropriate Declaration_Kinds:
   --    An_Ordinary_Type_Declaration
   --    A_Task_Type_Declaration
   --    A_Protected_Type_Declaration
   --    A_Private_Type_Declaration
   --    A_Private_Extension_Declaration
   --    A_Subtype_Declaration
   --    A_Formal_Type_Declaration
   --
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (applies on selector)
   --    An_Attribute_Reference


   function Is_Character_Subtype (The_Subtype : Asis.Element) return Boolean;
   -- Returns True if The_Subtype is a declaration of a character type, or a name of a character type
   -- Returns False in all other cases
   --
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name
   --    An_Expression

   function Is_Class_Wide_Subtype (The_Subtype : Asis.Element) return Boolean;
   -- Unwinds subtype declarations and returns true if the given subtype declaration,
   -- or the type designated by the provided name, ultimately designates a class-wide type.
   --
   -- Appropriate Element_Kinds:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name
   --    An_Expression
   --
   -- Appropriate Declaration_Kinds:
   --    A_Subtype_Declaration
   --
   -- Appropriate Definition_Kinds:
   --    A_Type_Definition (always returns false)
   --    A_Subtype_Indication
   --
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (applies on selector)
   --    An_Attribute_Reference

   function Is_Controlled (The_Element : Asis.Element) return Boolean;
   -- Returns True if The_Element is a defining_name, declaration or definition of a controlled
   -- type, of an object of a controlled type, or an expression whose type is controlled.
   -- Returns False in all other cases
   --
   -- Appropriate Element_Kinds
   --   A_Declaration
   --   A_Definition
   --   A_Defining_Name
   --   An_Expression
   --
   --  Appropriate Expression_Kinds:
   --       An_Identifier
   --       An_Operator_Symbol
   --       A_Selected_Component (applies to selector)

   function Is_Limited (The_Element : Asis.Element) return Boolean;
   -- Returns True if The_Element is a defining_name, declaration or definition of an (explicit or implicit)
   -- limited type, of an object of a limited type, or an expression whose type is limited.
   -- Returns False in all other cases
   --
   -- Appropriate Element_Kinds
   --   A_Declaration
   --   A_Definition
   --   A_Defining_Name
   --   An_Expression
   --
   --  Appropriate Expression_Kinds:
   --       An_Identifier
   --       An_Operator_Symbol
   --       A_Character_Literal
   --       An_Enumeration_Literal
   --       A_Selected_Component (applies to selector)


   function Corresponding_Full_Type_Declaration (Decl : Asis.Declaration) return Asis.Declaration;
   -- Returns the full type of Decl if Decl is an incomplete or private (tagged) type declaration
   -- Returns Decl otherwise
   --
   -- Appropriate Declaration_Kinds:
   --       A_Type_Declaration


   type Derivation_Descriptor is
      record
         Ultimate_Type    : Asis.Declaration;  -- The ancestor which is not a derived types
         Derivation_Depth : Asis.ASIS_Natural; -- How many derivations
         First_Constraint : Asis.Constraint;   -- The first constraint encountered when going up derivations
      end record;

   function Corresponding_Derivation_Description (The_Subtype : Asis.Declaration;
                                                  Privacy     : Privacy_Policy := Follow_User_Private)
                                                  return Derivation_Descriptor;
   -- Unwinds subtype declarations, derivations, private types and returns the real declaration
   -- that tells what the type really is, and how many declarations are encountered in the way
   -- (0 means that The_Subtype is not a derived type).
   --
   -- Note that for tagged types, derivations are also unwound up to the declaration that
   -- includes the word "tagged".
   -- Privacy defines behaviour when a private type is encountered during unwinding.
   -- Unwinding stops at formal types, even if they are derived formal types.
   --
   -- Appropriate Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration
   --       A_Subtype_Declaration
   --       A_Formal_Type_Declaration
   --
   --  Returns Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Formal_Type_Declaration
   --  Returns Type_Kinds:
   --       An_Enumeration_Type_Definition
   --       A_Signed_Integer_Type_Definition
   --       A_Modular_Type_Definition
   --       A_Floating_Point_Definition
   --       An_Ordinary_Fixed_Point_Definition
   --       A_Decimal_Fixed_Point_Definition
   --       An_Unconstrained_Array_Definition
   --       A_Constrained_Array_Definition
   --       A_Record_Type_Definition
   --       A_Tagged_Record_Type_Definition
   --       An_Access_Type_Definition


   function Ultimate_Type_Declaration (The_Subtype : Asis.Declaration;
                                       Privacy     : Privacy_Policy := Follow_User_Private)
                                       return Asis.Declaration;
   -- Returns the Ultimate_Type component of Corresponding_Derivation_Description

   function Derivation_Depth (The_Subtype : Asis.Declaration;
                              Privacy     : Privacy_Policy := Follow_User_Private)
                                       return Asis.ASIS_Natural;
   -- Returns the Derivation_Depth component of Corresponding_Derivation_Description

   function Is_Type_Declaration_Kind (The_Subtype : Asis.Declaration; The_Kind : Asis.Declaration_Kinds) return Boolean;
   -- Unwinds subtype declarations and derivations and returns true if the given subtype
   -- declaration ultimately designates a type of the given kind.
   --
   -- Appropriate Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration
   --       A_Subtype_Declaration
   --       A_Formal_Type_Declaration
   -- Appropriate Definition_Kinds:
   --       all definitions corresponding to the above

   function Contains_Type_Declaration_Kind (The_Subtype : Asis.Declaration;
                                            The_Kind    : Asis.Declaration_Kinds;
                                            The_Type    : Asis.Type_Kinds := Asis.Not_A_Type_Definition) return Boolean;
   -- Like Is_Type_Declaration_Kind, but for composite types, returns True if any subcomponent is
   -- of the given declaration kind and type kind.
   -- The_Type = Not_A_Type_Definition really means "ignore type kind", wich is of course the only sensible value
   --   if The_Kind /= An_Ordinary_Type_Declaration
   -- If The_Kind = An_Ordinary_Type_Declaration and The_Type = An_Access_Type_Definition, the function returns
   --    True if any subcomponent is of an anonymous access type (which, strictly speaking, is not
   --    An_Ordinary_Type_Declaration)
   --
   -- Appropriate Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration
   --       A_Subtype_Declaration
   --       A_Formal_Type_Declaration

   generic
      type State_Information is limited private;

      with procedure Pre_Operation  (Element : in     Asis.Definition;
                                     Control : in out Asis.Traverse_Control;
                                     State   : in out State_Information;
                                     Depth   : in     Asis.ASIS_Positive) is null;
      with procedure Post_Operation (Element : in     Asis.Definition;
                                     Control : in out Asis.Traverse_Control;
                                     State   : in out State_Information;
                                     Depth   : in     Asis.ASIS_Positive) is null;
   procedure Traverse_Data_Structure (Element : in     Asis.Element;
                                      Control : in out Asis.Traverse_Control;
                                      State   : in out State_Information);
   -- Calls Pre_Operation on the definition of Element, a type declaration or  a (possibly anonymous) type definition,
   -- and recursively to all type definitions of its subcomponents, then Post_Operation on the definition of Element.
   -- The element passed to Pre/Post operations is the type definition of the subtype indication of each component.
   -- Control works exactly as defined for Asis.Iterator.Traverse_Element.
   -- Recursion ignores privacy (i.e. we recurse through components of private types), but the Pre_Operation can return
   --    Ignore_Children on private types, thus stopping privacy breaks.
   -- Depth is the structural depth of the component: 1 for the initial Element, 2 for components of the corresponding
   --     type, etc.
   -- WARNING: Element is Nil_Element for the non-existent definition of a Task_Declaration without a Task_Definition


   type Discriminant_Part_Kinds is (No_Discriminant_Part,          A_Nondefaulted_Discriminant_Part,
                                    A_Defaulted_Discriminant_Part, An_Unknown_Discriminant_Part);
   function Discriminant_Part_Kind (Elem : Asis.Element) return Discriminant_Part_Kinds;
   -- Appropriate Element_Kinds:
   --     A_Declaration
   --     A_Definition
   --  Appropriate Declaration_Kinds:
   --     An_Ordinary_Type_Declaration
   --     A_Task_Type_Declaration
   --     A_Protected_Type_Declaration
   --     An_Incomplete_Type_Declaration
   --     A_Tagged_Incomplete_Type_Declaration
   --     A_Private_Type_Declaration
   --     A_Private_Extension_Declaration
   --     A_Formal_Type_Declaration
   --  Appropriate Definition_Kinds:
   --     An_Unknown_Discriminant_Part
   --     A_Known_Discriminant_Part
   --  Nil_Element is allowed and returns No_Discriminant_Part


   function Attribute_Clause_Expression (Attribute : in Asis.Attribute_Kinds;
                                         Elem      : in Asis.Element)
                                         return Asis.Expression;
   -- Returns the Expression of the attribute specification clause that applies
   -- to the indicated type or object (or declaration thereof)
   -- Returns Nil_Element if there is no applicable clause.
   -- If Attribute is 'Address or 'Alignment, Ada83 equivalent forms are considered
   --
   -- Warning: implementation relies on Corresponding_Representation_Clauses, and therefore
   --          cannot be trusted for objects.
   --
   -- Appropriate Element_Kinds:
   --     A_Declaration
   --     A_Defining_Name
   --     An_Expression
   --
   --  Appropriate Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component (applies to selector)
   --       An_Attribute_Reference (T'Base or T'Class)

   function Corresponding_Component_Clause (Component : in Asis.Defining_Name) return Asis.Component_Clause;
   -- Returns the component clause that applies to the indicated component.
   -- Returns Nil_Element if there is no applicable component clause.
   --
   -- We use the defining_name rather than the component declaration here to avoid problems in
   -- case of declarations with several defining names.

   function Corresponding_Enumeration_Clause (Enumeration_Value : in Asis.Defining_Name) return Asis.Association;
   -- Returns the association Enumeration_Value => Representation for the given Enumeration_Value
   -- Returns Nil_Element if there is no applicable enumeration representation clause


   type Type_Categories is (Not_A_Type,
                            An_Enumeration_Type,
                            A_Signed_Integer_Type,
                            A_Modular_Type,
                            A_Fixed_Point_Type,
                            A_Floating_Point_Type,
                            An_Array_Type,
                            A_Record_Type,               -- untagged
                            A_Tagged_Type,               -- including record extensions if Separate_Extension is false
                            An_Extended_Tagged_Type,
                            An_Access_Type,
                            A_Derived_Type,
                            A_Private_Type,
                            A_Task_Type,
                            A_Protected_Type);
   subtype Discrete_Types     is Type_Categories range An_Enumeration_Type   .. A_Modular_Type;
   subtype Numeric_Types      is Type_Categories range A_Signed_Integer_Type .. A_Modular_Type;
   subtype Scalar_Types       is Type_Categories range An_Enumeration_Type   .. A_Floating_Point_Type;
   subtype Integer_Types      is Type_Categories range A_Signed_Integer_Type .. A_Modular_Type;
   subtype Real_Types         is Type_Categories range A_Fixed_Point_Type    .. A_Floating_Point_Type;
   subtype Composite_Types    is Type_Categories range An_Array_Type         .. A_Tagged_Type;
   subtype Synchronized_Types is Type_Categories range A_Task_Type           .. A_Protected_Type;

   function Type_Category (Elem               : in Asis.Element;
                           Follow_Derived     : in Boolean := False;
                           Privacy            : in Privacy_Policy := Stop_At_Private;
                           Separate_Extension : in Boolean := False) return Type_Categories;
   -- For private and derived types:
   --    If Follow_Derived (resp. Follow_Private) is True, returns the category of the
   --    parent (full) type instead of A_Derived_Type (A_Private_Type).
   --    Formal private types return A_Private_Type even if Follow_Private is True (no corresponding full declaration)
   --
   -- For (private) type extensions:
   --   if Separate_Extension is True, returns An_Extended_Tagged_Type, otherwise returns A_Tagged_Type.
   --
   -- Incomplete types are always followed.
   --
   -- Expressions can be either type names or true (typed) expressions
   --
   -- Appropriate Element_Kinds:
   --       A_Declaration
   --       A_Definition
   --       A_Defining_Name
   --       An_Expression
   -- Appropriate Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration
   --       A_Subtype_Declaration
   --       A_Formal_Type_Declaration
   --       A_Variable_Declaration
   --       A_Constant_Declaration
   --       A_Deferred_Constant_Declaration
   --       A_Component_Declaration
   --       A_Discriminant_Specification
   -- Appropriate Definition_Types:
   --       A_Type_Definition
   --       A_Task_Definition
   --       A_Protected_Definition
   --
   -- Notes:
   -- 1- We do not distinguish between ordinary and decimal fixed point types, because we
   --    would be unable to know what to return for expressions that are universal_fixed
   -- 2- Handling of expressions of a root or universal type:
   --    Root_Integer and Universal_Integer returns A_Signed_Integer_Type
   --    Root_Real and Universal_Real returns A_Floating_Point_Type (which is the best we can do)
   --    Universal_Fixed returns A_Fixed_Point type, of course

   function Range_Ultimate_Name (Range_Def : Asis.Definition) return Asis.Defining_Name;
   -- Return the name of the type of the given range.
   -- Returns Nil_Element if the range is given by two integer literals (implicit Integer)
   --
   --  Expected Definition_Kinds:
   --       A_Discrete_Subtype_Definition
   --       A_Discrete_Range

   function Dimensionality (Type_Def : Asis.Type_Definition) return Asis.ASIS_Natural;
   -- Return the number of dimensions of a (constrained or unconstrained) array type.
   -- Returns 0 if the definition is not of an array type.
   --
   -- Appropriate type kinds:
   --       An_Unconstrained_Array_Definition
   --       A_Constrained_Array_Definition
   -- Appropriate formal type kinds:
   --       A_Formal_Unconstrained_Array_Definition
   --       A_Formal_Constrained_Array_Definition

   function Index_Subtypes_Names (Type_Def : Asis.Type_Definition) return Asis.Element_List;
   -- Return the list of defining names for the subtypes of an (constrained or unconstrained) array
   -- definition.
   -- Returns a nil element instead of a defining name in the case of e.g. array(1..10)
   --
   -- Appropriate type kinds:
   --       An_Unconstrained_Array_Definition
   --       A_Constrained_Array_Definition
   -- Appropriate formal type kinds:
   --       A_Formal_Unconstrained_Array_Definition
   --       A_Formal_Constrained_Array_Definition

   function Corresponding_Static_Predicates (Elem : in Asis.Element) return Asis.Element_List;
   -- Return the list of expressions of all applicable static predicates (directly given or inherited)
   -- Returns a Nil_Element_List if there are no applicable static predicate
   -- In case of an expression, returns applicable static predicates for its subtype.
   -- Returns Nil_Element_List for any inappropriate element
   --
   -- Appropriate element kinds:
   --      An_Expression
   --      A_Declaration

   function Corresponding_Static_Predicates (List : in Asis.Element_List) return Asis.Element_List;
   -- Return the catenation of Corresponding_Static_Predicates for all elements in List

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about names and expressions                                                         --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Simple_Name (The_Name : Asis.Expression) return Asis.Expression;
   -- Gets rid of selection, i.e. returns the selector of its argument if A_Selected_Component,
   -- its argument otherwise.

   function Unindexed_Name (The_Name : Asis.Expression) return Asis.Expression;
   -- Gets rid of indexing, i.e. returns the selector of its argument if An_Indexed_Component,
   -- its argument otherwise.

   function Name_Path (Name : Asis.Expression) return Asis.Element_List;
   -- For A_Selected_Component (to several levels), returns all components as a List (in textual order)
   -- Otherwise, returns Name as a one element list

   function Strip_Attributes (Name : Asis.Expression) return Asis.Expression;
   -- If Name is an Attribute_Reference, returns the first prefix which is not itself an Attribute_Reference.
   -- Returns Name otherwise

   function Strip_Parentheses (Expr : Asis.Expression) return Asis.Expression;
   -- If Expr is a parenthesized expression, removes all levels of parentheses,
   -- otherwise returns Expr

   function Ultimate_Name (The_Name : Asis.Element; No_Component : Boolean := False) return Asis.Element;
   -- For a name defined by a renaming declaration: returns the name of the entity, which is not
   --   itself defined by a renaming.
   --   - In the case of a renaming whose target is part of a record (or protected) type:
   --        if No_Component is False, returns the name of the component
   --        if No_Component is True, returns the name of the object the field belongs to
   --   - In the case of a renaming whose target is part of an array, returns the name of
   --     array object
   --        (i.e. X : T renames V.Field (2) => Field if No_Component is false,
     --                                      => V if No_Component is True).
   --   - In the case of a renaming whose target is An_Explicit_Dereference, returns Nil_Element
   --     (the target is statically unknown)
   --   - In the case of a renaming whose target is A_Function_Call, returns Nil_Element
   --     (the target is statically unknown, it designates the result of the function call)
   --   - In the case of the renaming of an attribute, returns the attribute
   -- Otherwise: returns its argument (including A_Nil_Element)
   --
   -- In any case, if The_Name is A_Defining_Name, then A_Defining_Name is returned
   -- if The_Name is An_Identifier, then An_Identifier (or An_Attribute_Reference) is returned,
   --   and always as a simple name.
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (operates on selector)
   --    An_Attribute_Reference

   function First_Defining_Name (Name : Asis.Element) return Asis.Defining_Name;
   -- Returns the first textually encountered defining name of Name (assuming normal
   -- processing order), i.e.:
   -- For a program unit with a spec: the defining name from the spec, given the name
   --     of a spec, the name of a body, the name of a stub, or the name of a proper body
   -- For a subprogram without a spec: the defining name from the body (given same input)
   -- For a private or incomplete or deferred declaration, or the full declaration of one
   --     of these: the defining name of the private or incomplete or deferred declaration.
   -- For a formal parameter of a body: the corresponding formal parameter of the spec if
   --     there is one, the one from the body otherwise.
   -- For a formal parameter of a proper body: the corresponding formal parameter of the spec if
   --     there is one, the one from the stub otherwise.
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (operates on selector)

   function Matching_Name (Name : Asis.Defining_Name; Decl : Asis.Declaration) return Asis.Defining_Name;
   -- Return the Defining_Name from Decl which is identical to Name
   -- return Nil_Element if not found

   function Ultimate_Expression (Expr : Asis.Expression) return Asis.Expression;
   -- Returns Simple_name (Expr), unless Expr is the name of constant, in which case it returns
   -- the Ultimate_Expression of the initialization expression of the constant.

   function Association_Choices (Assoc : Asis.Association) return Asis.Expression_List;
   -- Returns the list of choices (LHS) of the association, independently of the association kind.
   -- Returns Nil_Element_List for a positional association.

   function Association_Value (Assoc : Asis.Association) return Asis.Expression;
   -- Returns the value (RHS) of the association, independently of the association kind.

   function Is_Static_Object (Obj : Asis.Expression) return Boolean;
   -- Return True if Obj is a name that designates a statically determinable object
   -- (including, f.e., statically indexed array components).
   -- Return False in all other cases, including when Obj does not designate an Object


   function Used_Identifiers (Name : Asis.Expression) return Asis.Expression_List;
   -- Return all identifiers corresponding to entities accessed by the use of the name.
   -- For identifiers that are not renamings, returns Name in a list of length 1
   -- For identifiers declared by renaming, returns Name plus every record field and object name which
   -- is part of the renamed expression, but not parts that are evaluated by the
   -- elaboration of the renaming declaration itself. In short, if we see a name "Ren" declared by:
   --    Ren : Integer renames Integer'(Pack.Tab(I).F);
   -- we must consider that when Ren is used, Tab (a variable) is used, and F (a record field)
   -- is used, but not Pack (a package) nor I (evaluated by the elaboration of the renaming),
   -- nor Integer (evaluated by the elaboration of the renaming).
   -- Of course, this is recursive. Given:
   --    Ren : Integer renames A.I;
   --    A   : Rec renames B;
   --    B   : Rec;
   -- The returned list is (Ren, A, B, I)
   -- Lower bound of result is always 1, and the first element of the list Is_Identical to Name
   --
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    An_Operator_Symbol
   --    An_Enumeration_Literal


   function Corresponding_Expression_Type_Definition (The_Element : Asis.Expression) return Asis.Definition;
   -- return the (full) type definition of the type of The_Element
   -- Unlike Corresponding_Expression_Type, works if the the type of The_Element is
   -- an anonymous type and unwinds subtypes (but not derived types)
   -- This query is a candidate for ASIS05, and already provided by ASIS-for-GNAT,
   -- but we don't use it as long as it is not standard
   --
   -- WARNING 1: When passed a slice, returns the definition of the sliced object, bounds
   --            will be those of the sliced object, not the slice itself. But what else
   --            can we do?
   -- WARNING 2: This function works when passed a type name (not really an expression),
   --            not sure if it will work with the ASIS05 query

   function Corresponding_Components (The_Element : Asis.Element) return Asis.Record_Component_List;
   -- If the element is (a defining name of) a variable or a component of a record type, returns the
   -- corresponding components
   -- Return Nil_Element_List otherwise

   function Ultimate_Expression_Type (The_Element : Asis.Expression) return Asis.Definition;
   -- return the type definition of the ultimate ancestor type of The_Element
   -- (going up all subtype and derived type declaration, and through private and incomplete declarations).


   function Expression_Type_Kind (The_Element : Asis.Expression) return Asis.Type_Kinds;
   -- Real kind of an expression
   -- return the Type_Kind of the ultimate ancestor of The_Element
   -- (going up all subtype and derived type declaration).

   function Is_Access_Expression (The_Element : Asis.Expression) return Boolean;
   -- Return True if The_Element is of an access type or of a formal access type.
   -- Return False in all other cases

   type Expression_Usage_Kinds is (Untouched, Read, Write, Read_Write, Unknown);
   function Expression_Usage_Kind (Expr : Asis.Expression) return Expression_Usage_Kinds;
   -- Returns Untouched if Expr is part of a renaming declaration or the prefix of an attribute
   -- Returns Write if Expr designates a variable which is the
   --  target of an assignment statement, or an actual corresponding
   --  to an out parameter in a procedure or entry call.
   -- Returns Read_Write if Expr designates a variable which is
   --  an actual corresponding to an in out parameter in a procedure
   --  or entry call.
   -- Returns Unknown when usage cannot be determined (parameter of a dispatching call)
   -- Returns Read in all other cases (including when Expr is not a variable)
   --
   -- Note that this function handles access types properly, i.e. in:
   --    Integer_Pointer.all := 1;
   -- if passed Integer_Pointer.all, it will return Write;
   -- if passed Integer_Pointer, it will return Read.

   function Includes_Renaming (Path : Asis.Expression) return Boolean;
   -- Checks whether any element in the Path is a renaming
   --
   -- Appropriate expression kinds:
   --   An_Identifier
   --   A_Selected_Component
   --   A_Function_Call
   --   An_Indexed_Component
   --   A_Slice


   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about pragmas                                                                       --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   type Pragma_Set is array (Asis.Pragma_Kinds) of Boolean;
   function Corresponding_Pragma_Set (Element : in Asis.Element) return Pragma_Set;
   -- Returns the set of pragmas that apply to the corresponding name or defining name
   -- (including, for an object, those inherited from its type).
   -- Pragmas that apply to T'Class are not included to the set for T (due to ASIS weakness)
   -- Note that unlike Corresponding_Pragmas, this query makes sure that the pragma applies
   -- really to the given element in the case of a multiple declaration.
   --
   -- Appropriate element kinds:
   --   An_Expression
   --   A_Defining_Name
   -- Appropriate expression kinds
   --   An_Identifier
   --   A_Selected_Component (function applied to the selector)


   function Is_Profile_Applied (Element : in Asis.Element; Profile : Wide_String) return Boolean;
   -- Returns True if Profile applies to Element.
   -- Profile must be given in upper case.
   --
   -- Appropriate element kinds:
   --   Any element


   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about callable constructs                                                           --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   type Callable_Kinds is (Not_A_Callable,          A_Procedure_Callable,  A_Function_Callable,
                           An_Enumeration_Callable, A_Task_Entry_Callable, A_Protected_Entry_Callable);
   subtype A_Subprogram_Callable is Callable_Kinds range A_Procedure_Callable  .. A_Function_Callable;
   subtype An_Entry_Callable     is Callable_Kinds range A_Task_Entry_Callable .. A_Protected_Entry_Callable;

   function Callable_Kind (Element : Asis.Element) return Callable_Kinds;
   -- Checks whether the Element is a callable construct, and which one
   -- Expected elements:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    An_Attribute_Reference
   --    A_Selected_Component (applies to the selector)

   function Is_Callable_Construct (Element : Asis.Element) return Boolean;
   -- Checks whether the Element is a callable construct
   -- Expected elements like Callable_Kind

   function Is_Task_Entry (Declaration : Asis.Declaration) return Boolean;
   -- Returns True if the Declaration is An_Entry_Declaration of a task.
   -- Returns False if it is An_Entry_Declaration of a protected type
   -- Expected elements:
   --    A_Declaration
   -- Appropriate Declaration_Kinds:
   --    An_Entry_Declaration

   function Is_Predefined_Operator (Decl : Asis.Declaration) return Boolean;
   -- Expected declaration kind:
   --    A_Function_Declaration
   --    A_Function_Body_Declaration
   -- (of operator)
   -- Returns True if the operator is identical to a predefined one.

   function Called_Simple_Name (Call : Asis.Element) return Asis.Expression;
   -- Given a procedure, entry or function call, returns the simple name of the called
   -- entity (from the call).
   -- It handles all cases in the same function (i.e. whether it is a procedure or a function,
   -- whether the call is from an access to subprogram, etc.)
   -- Returns the simple name of the called entity (i.e. not a Selected_Name).
   -- For calls to an entry family, returns the name of the family
   -- Returns a Nil_Element if the call is through an access to subprogram
   -- Works with dispatching calls!
   --
   -- Appropriate Element_Kinds:
   --    A_Statement
   --    An_Expression
   -- Appropriate Statement_Kinds:
   --    A_Procedure_Call_Statement
   --    An_Entry_Call_Statement
   -- Appropriate Expression_Kinds:
   --    A_Function_Call


   type Call_Kind is (A_Regular_Call,     A_Predefined_Entity_Call, An_Attribute_Call,
                      A_Dereference_Call, A_Dispatching_Call,       An_Enumeration_Literal);
   type Call_Descriptor (Kind : Call_Kind := A_Regular_Call) is
      record
         case Kind is
            when A_Regular_Call =>
               Declaration : Asis.Declaration;
            when others =>
               null;
         end case;
      end record;
   function Corresponding_Call_Description (Call : Asis.Element) return Call_Descriptor;
   -- Given a procedure, entry or function call, returns the descriptor of the called
   -- entity.
   -- If the call is to a good ol' statically determinable callable entity, the descriptor
   -- is A_Regular_Call, and the Declaration field contains the real declaration, after unwinding
   -- all possible renamings.
   -- Otherwise, the Kind discriminant tells why the declaration is not available.
   --
   -- Appropriate Element_Kinds:
   --    A_Statement
   --    An_Expression
   -- Appropriate Statement_Kinds:
   --    A_Procedure_Call_Statement
   --    An_Entry_Call_Statement
   -- Appropriate Expression_Kinds:
   --    A_Function_Call

   function Called_Profile (Call : Asis.Element) return Asis.Parameter_Specification_List;
   -- Given a procedure, entry or function call, returns the parameter profile of the called
   -- entity.
   -- For simple cases, it is like Parameter_Profile (Corresponding_Called_Entity (Call)),
   -- but it handles all cases in the same function (i.e. whether it is a procedure or a function,
   -- whether the call is from an access to subprogram, etc.)
   --
   -- Appropriate Element_Kinds:
   --    A_Statement
   --    An_Expression
   -- Appropriate Statement_Kinds:
   --    A_Procedure_Call_Statement
   --    An_Entry_Call_Statement
   -- Appropriate Expression_Kinds:
   --    A_Function_Call


   type Type_Attribute is (None, Base, Class);
   type Profile_Descriptor (Formals_Length : Asis.ASIS_Natural);
   type Profile_Access is access Profile_Descriptor;
   type Profile_Entry is
      record
         Access_Form  : Asis.Access_Definition_Kinds;
         Attribute    : Type_Attribute;
         Name         : Asis.Defining_Name;
         Anon_Profile : Profile_Access;
      end record;
   type Profile_Table is array (Asis.List_Index range <>) of Profile_Entry;
   type Profile_Descriptor (Formals_Length : Asis.ASIS_Natural) is
      record
         Result_Type : Profile_Entry;
         Formals     : Profile_Table (1 .. Formals_Length);
      end record;

   function Types_Profile (Declaration : in Asis.Element)  return Profile_Descriptor;
   -- Given a callable entity declaration, returns a description of the profile
   -- type Profile_Descriptor:
   --    Result_Type describes the result *type* for a function, Nil_Element for other callable entities
   --    Formals are (in order of declaration) the *types* of the parameters.
   --       Multiple declarations are separated, i.e. "A,B : Integer" yields two entries in the table.
   -- type Profile_Entry:
   --    Access_Form : for anonymous access parameters: the kind of anonymous access (or Not_An_Access_Definition)
   --    Attribute   : The kind of attribute used with the type name, if any
   --    Name        : the type name, stripped off of any decoration (access, 'Base, 'Class)
   --                  only for non-anonymous access types and anonymous access to object types (otherwise nil_element)
   --    Anon_Profile: only for anonymour access to subprograms, the profile pointed to. Since these are supposed to be
   --                  pretty (!) rare, we don't care about deallocating the corresponding structure
   --                  (yes, it is a deliberate memory leak).
   --
   -- Note that Non-nil Name and Non-nil Anon_Profile are exclusive, but we didn't put them as variants,
   -- because this would have required a discriminant (sometimes evaluated dynamically, preventing aggregates) and
   -- would complicate making arrays of Profile_Entry... Too much burden to save a single word.
   --
   -- Appropriate Element_Kinds:
   --   A_Declaration
   --
   -- Appropriate Declaration_Kinds:
   --   Any (generic) callable entity declaration or body, or an anonymous access to subprogram definition


   function All_Formals (Profile : Asis.Parameter_Specification_List) return Asis.Defining_Name_List;
   -- Builds a list of all Defining_Name in the profile, "flattening" multiple declarations


   function Formal_Name (Call : Asis.Element; Actual : Asis.List_Index) return Asis.Defining_Name;
   -- Given a procedure, entry or function call, or a generic instantiation, returns the defining name
   -- of the formal corresponding to the actual at the given position in the call.
   -- Not to be called on an actual which is An_Others_Choice_Specification, since it may correspond to
   -- several formals.
   -- Note: if the full Parameter_Specification is desired, it is the Enclosing_Element of the Formal_Name
   --
   -- Returns a nil element if:
   --   Actual is greater than the number of actuals in the call
   --   The call is to a dispatching operation


   function Formal_Name (Assoc : Asis.Association) return Asis.Defining_Name;
   -- Same as above, but retrieves the call (or instantiation) and the position given an association
   -- Does not work if Assoc has been obtained from a normalized association!


   function Matching_Formal_Name (Name : Asis.Defining_Name; Into : Asis.Declaration) return Asis.Defining_Name;
   -- Given the defining name of a formal parameter of a callable entity, returns the defining name
   -- of the same parameter from the provided other part of the callable entity (spec, body, stub)


   function Actual_Expression (Call           : Asis.Element;
                               Formal         : Asis.Defining_Name;
                               Return_Default : Boolean := True) return Asis.Expression;
   -- Given a procedure, entry or function call, or a generic instantiation, returns the value
   -- of the actual corresponding to the formal whose defining_identifier is passed.
   -- If there is no such actual (the call used the default value) and Return_Default is True,
   -- the default expression is returned.
   --
   -- Returns a nil element if:
   --   The call is to a dispatching operation
   --   The formal is not from the called entity
   --   The call used the default value and Return_Default is False


   function Actual_Parameters (Element : Asis.Element; Normalized : Boolean := False) return Asis.Association_List;
   -- Returns the actual parameters of a procedure, entry, or function call, or of
   -- a generic instantiation


   function External_Call_Target (Call : Asis.Element) return Asis.Expression;
   -- Returns the prefix of the call that designates the target object
   -- of an external call (LRM 9.5 (2..7)).
   -- Returns Nil_Element if Element does not designate an external call.
   --
   -- Appropriate Element_Kinds:
   --   An_Expression
   --   A_Statement
   --
   -- Appropriate Expression_Kinds:
   --   A_Function_Call
   --
   -- Appropriate Statement_Kinds:
   --   A_Procedure_Call_Statement
   --   An_Entry_Call_Statement
   --   A_Requeue_Statement
   --   A_Requeue_Statement_With_Abort


   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Static evaluator and static properties of ranges and constraints                            --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   type Extended_Biggest_Int is range System.Min_Int .. System.Max_Int; -- The best we can do
   Not_Static : constant Extended_Biggest_Int := Extended_Biggest_Int'Last;

   subtype Biggest_Int              is Extended_Biggest_Int range Extended_Biggest_Int'First .. Not_Static - 1;
   subtype Biggest_Natural          is Biggest_Int          range 0 .. Biggest_Int'Last;
   subtype Biggest_Positive         is Biggest_Int          range 1 .. Biggest_Int'Last;
   subtype Extended_Biggest_Natural is Extended_Biggest_Int range 0 .. Not_Static;

   function Biggest_Int_Img (Item : Biggest_Int) return Wide_String;
   -- Like Biggest_Int'Wide_Image, without the !*#!! initial space.
   -- (avoids depending on the Gnat specific attribute 'Img)

   type Extended_Biggest_Int_List is array (Asis.List_Index range <>) of Extended_Biggest_Int;
   Nil_Extended_Biggest_Int_List : constant Extended_Biggest_Int_List (1 .. 0) := (others => 0);
   function "=" (Left, Right : Extended_Biggest_Int_List) return Boolean;
   -- Like the regular "=", except that it returns false if there is a Not_Static in either parameter

   type Extended_Biggest_Natural_List is array (Asis.List_Index range <>) of Extended_Biggest_Natural;
   Nil_Extended_Biggest_Natural_List : constant Extended_Biggest_Natural_List (1 .. 0) := (others => 0);

   type Biggest_Float is digits System.Max_Digits; -- The same for floatting point numbers

   function Static_Expression_Value_Image (Expression : Asis.Expression) return Wide_String;
   --  Computes the value of Expression if it is a static expression
   --  and represents it as a (wide) string. For enumeration expressions, the
   --  image of the Pos value of the defining enumeration or character literal
   --  corresponding to the  value of the expression is returned.
   --  There is NO leading space for positive values!
   --
   --  For non-static expressions, or expressions that we cannot (yet) evaluate,
   --  an empty string is returned.
   --
   --  Currently implemented:
   --  All types:
   --     Constant
   --     Parenthesized expression
   --     'Pred, 'Succ, 'Pos, 'Val, 'First, 'Last
   --     'Size if specified by a size clause, or a standard type, or a type derived type from one of these
   --     Conversions and qualified expressions
   --     Comparison operators
   --  Integer: (provided values are within System.Min_Int .. System.Max_Int)
   --     Literal
   --     Named number
   --     + - * / **
   --  Real:
   --     Literal
   --     Named number
   --     + - * / **
   --  Enumerated:
   --     Literal
   --  String: (no way to distinguish true "" from non-static expression)
   --     Literal
   --     &
   --
   --  Appropriate Element_Kinds:
   --     An_Expression
   --
   --  The specification of this function is derived (and compatible with) the one
   --  declared in the GNAT extension ASIS.Extensions
   --  (except that we do not have the same set of implemented/non-implemented features)


   function Discrete_Static_Expression_Value (Expression : Asis.Expression) return Extended_Biggest_Int;
   -- Like Static_Expression_Value_Image, but returns the actual value for static discrete expressions.
   -- Returns Not_Static for other cases

   function Is_Static_Expression (Expr : Asis.Expression) return Boolean;
   -- Returns True if Expr is a static expression
   -- TBH: if it is a static expression that Static_Expression_Value_Image is able to evaluate

   function Size_Value_Image (Name : Asis.Element) return Wide_String;
   -- Name must be the name of a type or object
   --
   -- For a type:
   --   - If a Size clause applies to the type, returns the value from the clause (including
   --     when the clause is inherited from some ancestor)
   --   - returns the host value for usual predefined types (Boolean, [Wide_[Wide_]Character,
   --     [Long_]Integer, [Long_]Float). For a cross-compiler, this may differ from the target!
   -- For a stand-alone object:
   --   - If a Size clause applies to the object, returns the value from the clause
   -- For a record component
   --   - if the component has a component clause from a record representation clause, returns
   --     the size computed from the component clause
   -- For an array component
   --   - If there is a Component_Size clause for the type of the array, returns the value from
   --     the clause
   -- In all other cases:
   --   - returns ""
   --
   -- Appropriate Element_Kinds:
   --   A_Defining_Name
   --   An_Expression
   --
   -- Appropriate Expression_Kinds:
   --   An_Identifier
   --   A_Selected_Component (applies to selector)
   --   An_Indexed_Component


   function Size_Value (Name : Asis.Expression) return Extended_Biggest_Int;
   -- Name must be the name of a type or object
   -- Like Size_Value_Image, but returns the actual value.
   -- Returns Not_Static if Size_Value_Image is ""


   function Constraining_Definition (E : Asis.Element) return Asis.Definition;
   -- Like Corresponding_Last_Constraint, except that it does not unwind subtypes at least once,
   -- accepts pretty much anything on input, and returns the first definition that imposes a constraint.
   --
   -- Appropriate Element_Kinds:
   --   An_Expression
   --   A_Declaration
   --   A_Definition
   --   A_Defining_Name


   function Discrete_Constraining_Bounds (Elem          : Asis.Element;
                                          Follow_Access : Boolean := False)
                                          return Asis.Element_List;
   -- Elem must designate a type or subtype, a variable, a constant, a formal parameter,
   -- a range, or a generic formal object.
   --
   -- Discrete or real type:
   -- Returns the expressions that constrain the values of a discrete or real type.
   -- Returned list has two elements
   -- Enumerated type     : returns (First_Defining_Name, Last_Defining_Name)
   -- Signed integer type : returns (First_Expression, Last_Expression)
   -- Subtype of modular  : returns (First_Expression, Last_Expression)
   -- Modular type        : returns (Nil_Element, Mod_Expression)
   -- Real type           : returns (First_Expression, Last_Expression) if range explicitely specified
   --                               (Nil_Element, Nil_Element) otherwise
   -- The expressions are replaced by Nil_Element if they cannot be determined (formal type, root type)
   --
   -- Arrays:
   -- Returns the expressions that constrain the indexes of an array object or type.
   -- Returned list has an even number of elements (First(1), Last (1), First (2), Last (2), ...)
   -- Each pair of elements is the same as above
   --
   -- Access types:
   -- Returns the Discrete_Constraining_Bounds of the accessed type if Follow_Access is True,
   -- Nil_Element_List otherwise
   --
   -- Others:
   -- Returns Nil_Element_List if the type that applies to Elem is not discrete or array
   --
   -- Appropriate Element_Kinds:
   --   An_Expression
   --   A_Declaration
   --   A_Definition
   --   A_Defining_Name
   --
   -- Appropriate Expression_Kind:
   --   An_Identifier
   --   A_Selected_Component (operates on the selector)
   --   A_Function_Call (operates on the returned object)
   --
   -- Appropriate Declaration_Kinds
   --   An_Ordinary_Type_Declaration
   --   A_Subtype_Declaration
   --   A_Task_Type_Declaration
   --   A_Protected_Type_Declaration
   --   A_Private_Extension_Declaration
   --   An_Incomplete_Type_Declaration
   --   A_Private_Type_Declaration
   --   A_Variable_Declaration
   --   A_Constant_Declaration
   --   A_Component_Declaration
   --   A_Parameter_Specification
   --   A_Formal_Object_Declaration
   --
   -- Appropriate Definition_Kinds
   --   A_Discrete_Range
   --   A_Constraint
   --   A_Type_Definition, appropriate Type_Kinds:
   --      An_Unconstrained_Array_Definition
   --      A_Constrained_Array_Definition
   --   A_Formal_Type_Definition, appropriate Formal_Type_Kinds:
   --      A_Formal_Unconstrained_Array_Definition
   --      A_Formal_Constrained_Array_Definition   --
   -- Returns Element_Kind:
   --   Not_An_Element
   --   An_Expression
   --   A_Defining_Name


   function Discrete_Constraining_Values (Elem          : Asis.Element;
                                          Follow_Access : Boolean := False)
                                          return Extended_Biggest_Int_List;
   -- Like Discrete_Constraining_Bounds, but returns the actual values of the bounds
   -- if statically determinable.
   -- Returns Not_Static if not statically determinable


   function Discrete_Constraining_Lengths (Elem          : Asis.Element;
                                           Follow_Access : Boolean := False)
                                           return Extended_Biggest_Natural_List;
   -- Like Discrete_Constraining_Bounds, but returns the number of values in the range instead of
   -- the bounds if statically determinable
   -- Returns Not_Static (-1) if not statically determinable

   function Are_Matching_Subtypes (Left, Right : Asis.Element) return Boolean;
   -- Determines if Left and Right are statically matching subtypes, as defined in 4.9.1
   -- Implemented for the moment: only the case of two subtypes comming from the same elaboration
   --
   -- If Left or Right is the name of an object, operates on its Corresponding_Expression_Type.
   -- Otherwise, Left and Right are expected to be (sub)type names.
   --
   -- Appropriate Element_Kinds:
   --   An_Expression
   --   A_Defining_Name
   --   A_Declaration
   --   A_Definition
   -- Appropriate Expression_Kind:
   --   An_Identifier
   --   A_Selected_Component (operates on the selector)
   --
   -- Returns False for any unexpected (or yet unimplemented) element


   type Result_Confidence is (Unlikely, Possible, Certain);
   type Variable_Overlap  is (Complete, Partial, None);
   type Proximity is
      record
         Confidence : Result_Confidence;
         Overlap    : Variable_Overlap;
      end record;
   Same_Variable       : constant Proximity := (Certain, Complete);
   Different_Variables : constant Proximity := (Certain, None);

   function Variables_Proximity (Left, Right : Asis.Element) return Proximity;
   -- Determines if Left and Right can possibly refer to (part of) the same variables.
   -- If Left or Right is not a variable, always returns (Certain, None)
   -- Overlap => None is only returned with Confidence => Certain
   --
   -- Some especially useful results:
   -- (Certain, Complete): Statically known to be the same variable
   -- (Certain, None)    : Statically known to be different variables
   --
   -- Appropriate Element_Kinds:
   --   An_Expression
   --   A_Defining_Name

   function Same_Value (Left, Right : Asis.Expression) return Boolean;
   -- Determines if Left and Right statically have the same value.
   -- Returns True if:
   --   Left and Right statically denote the same constant or in parameter
   --   or Left and Right are discrete and evaluate statically to the same value.

end Thick_Queries;
