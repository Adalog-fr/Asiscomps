----------------------------------------------------------------------
--  Thick_Queries - Package specification                           --
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

   function Ultimate_Enclosing_Instantiation (The_Element : Asis.Element) return Asis.Declaration;
   -- For an entity which Is_Part_Of_Instance:
   -- Return the "true" instantiation, i.e. the one written by the user, going up instantiations
   -- that appear in generics.


   function Is_Part_Of_Generic (Element : in Asis.Element) return Boolean;
   -- Checks whether the Element is included (directly or indirectly) in a generic
   --
   --  Appropriate Element_Kinds:
   --     Any element


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


   function Statements (Element : in Asis.Element) return Asis.Statement_List;
   -- Returns the statements of any construct with statements.
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

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Images                                                                                      --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Attribute_Name_Image (Attribute : Asis.Expression) return Wide_String;
   --  Like Pragma_Name_Image, but for an Attribute_Reference
   --
   --  Appropriate Element_Kinds:
   --    An_Expression
   --  Appropriate Expression_Kinds:
   --    An_Attribute_Reference


   function Extended_Name_Image (Name_Elem : Asis.Element) return Wide_String;
   -- Image of a Name or Defining_Name, given either as a simple name or as a Selected_Name


   function Full_Name_Image (The_Name     : in Asis.Element;
                             With_Profile : in Boolean := False) return Wide_String;
   -- Full name of a name
   -- Works like Asis.Declarations.Defining_Name_Image (or Name_Image),
   -- but returns the full (unique) name of The_Name, starting from the
   -- enclosing compilation unit (Standard for predefined elements).
   -- If With_Profile is true, "mangles" the name with a profile to provide a name
   -- that is unique even if overloaded.
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (returns the image of the selector)


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

   function Is_Class_Wide_Subtype (The_Subtype : Asis.Declaration) return Boolean;
   -- Unwinds subtype declarations and returns true if the given subtype declaration
   -- ultimately designates a class-wide type.
   --
   -- Appropriate Declaration_Kinds:
   --    A_Subtype_Declaration

   function Is_Limited (The_Subtype : Asis.Declaration) return Boolean;
   -- Returns True if The_Subtype is a declaration of an (explicit or implicit) limited type
   -- Returns False in all other cases

   function Ultimate_Type_Declaration (The_Subtype : Asis.Declaration) return Asis.Declaration;
   -- Unwinds subtype declarations, derivations, private types and returns the real declaration
   -- that tells what the type really is!
   -- Note that for tagged types, derivations are also unwound up to the declaration that
   -- includes the word "tagged".
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

   function Is_Type_Declaration_Kind (The_Subtype : Asis.Declaration; Kind : Asis.Declaration_Kinds) return Boolean;
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


   function Contains_Type_Declaration_Kind (The_Subtype : Asis.Declaration;
                                            Kind        : Asis.Declaration_Kinds) return Boolean;
   -- Like Is_Type_Declaration_Kind, but for composite types, returns True if any subcomponent is
   -- of the given kind.
   --
   -- Appropriate Declaration_Kinds:
   --       An_Ordinary_Type_Declaration
   --       A_Task_Type_Declaration
   --       A_Protected_Type_Declaration
   --       A_Private_Type_Declaration
   --       A_Private_Extension_Declaration
   --       A_Subtype_Declaration
   --       A_Formal_Type_Declaration


   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about names and expressions                                                         --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Ultimate_Name (The_Name : Asis.Element) return Asis.Element;
   -- For a name defined by a renaming declaration: returns the name of the entity, which is not
   --   itself defined by a renaming.
   --     of the field for A_Selected_Component and the name of the array for An_Indexed_Component
   --        (i.e. X : T renames V.Field (2) => Field).
   --   - In the case of a renaming whose target is An_Explicit_Dereference, returns Nil_Element
   --     (the target is statically unknown)
   --   - In the case of a renaming whose target is A_Function_Call, returns Nil_Element
   --     (the target is statically unknown, it designates the result of the function call)
   --   - In the case of the renaming of an attribute, returns the attribute
   -- Otherwise: returns its argument (including A_Nil_Element)
   --
   -- In any case, if The_Name is A_Defining_Name, then A_Defining_Name is returned
   -- if The_Name is An_Identifier, then An_Identifier (or An_Attribute_Reference) is returned
   --
   -- Appropriate Element_Kinds:
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (operates on selector)
   --    An_Attribute_Reference


   function Ultimate_Expression_Type (The_Element : Asis.Expression) return Asis.Definition;
   -- return the type definition of the ultimate ancestor type of The_Element
   -- (going up all subtype and derived type declaration).


   function Expression_Type_Kind (The_Element : Asis.Expression) return Asis.Type_Kinds;
   -- Real kind of an expression
   -- return the Type_Kind of the ultimate ancestor of The_Element
   -- (going up all subtype and derived type declaration).


   type Expression_Usage_Kinds is (Untouched, Read, Write, Read_Write);
   function Expression_Usage_Kind (Expr : Asis.Expression) return Expression_Usage_Kinds;
   -- Returns Untouched if Expr is part of a renaming declaration
   -- Returns Write if Expr designates a variable which is the
   --  target of an assignment statement, or an actual corresponding
   --  to an out parameter in a procedure or entry call.
   -- Returns Read_Write if Expr designates a variable which is
   --  an actual corresponding to an in out parameter in a procedure
   --  or entry call.
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

   type Pragma_Set is array (Asis.Pragma_Kinds) of Boolean;
   function Corresponding_Pragma_Set (Element : in Asis.Element) return Pragma_Set;
   -- Returns the set of pragmas that apply to the corresponding name or defining name
   -- Note that unlike Corresponding_Pragmas, this query makes sure that the pragma applies
   -- really to the given element in the case of a multiple declaration.
   --
   -- Appropriate element kinds:
   --   An_Expression
   --   A_Defining_Name
   -- Appropriate expression kinds
   --   An_Identifier
   --   A_Selected_Component (function applied to the selector)

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Queries about callable constructs                                                           --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   function Is_Callable_Construct (Element : Asis.Element) return Boolean;
   -- Checks whether the Element is a callable construct
   -- Expected elements:
   --    A_Declaration
   --    A_Definition
   --    A_Defining_Name
   --    An_Expression
   -- Appropriate Expression_Kinds:
   --    An_Identifier
   --    A_Selected_Component (applies to the selector)


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


   function Formal_Name (Call : Asis.Element; Actual : Asis.List_Index) return Asis.Defining_Name;
   -- Given a procedure, entry or function call, or a generic instantiation, returns the defining name
   -- of the formal corresponding to the actual at the given position in the call.
   -- Note: if the full Parameter_Specification is desired, it is the Enclosing_Element of the Formal_Name
   --
   -- Returns a nil element if:
   --   Actual is greater than the number of actuals in the call
   --   The call is to a dispatching operation


   function Formal_Name (Assoc : Asis.Association) return Asis.Defining_Name;
   -- Same as above, but retrieves the call (or instantiation) and the position given an association


   function Actual_Expression (Call : Asis.Element; Formal : Asis.Defining_Name) return Asis.Expression;
   -- Given a procedure, entry or function call, or a generic instantiation, returns the value
   -- of the actual corresponding to the formal whose defining_identifier is passed.
   -- If there is no such actual (the call used the default value), the default expression is returned.
   --
   -- Returns a nil element if:
   --   The call is to a dispatching operation
   --   The formal is not from the called entity


   function Actual_Parameters (Element : Asis.Element; Normalized : Boolean := False) return Asis.Association_List;
   -- Returns the actual parameters of a procedure, entry, or function call, or of
   -- a generic instantiation


   type Type_Attribute is (None, Base, Class);
   type Profile_Entry is
      record
         Is_Access : Boolean;
         Attribute : Type_Attribute;
         Name      : Asis.Defining_Name;
      end record;
   type Profile_Table is array (Asis.List_Index range <>) of Profile_Entry;
   type Profile_Descriptor (Formals_Length : Asis.ASIS_Natural) is
      record
         Result_Type : Profile_Entry;
         Formals     : Profile_Table (1..Formals_Length);
      end record;
   function Types_Profile (Declaration : in Asis.Declaration) return Profile_Descriptor;
   -- Given a callable entity declaration, returns a list of Defining_Name
   -- First element is the result type for a function, Nil_Element for other callable entities
   -- Other elements are (in order of declaration) the *types* of the parameters.
   -- Multiple declarations are separated, i.e. "A,B : Integer" yields two entries in the table.


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
   --   A_Reqeue_Statement
   --   A_Requeue_Statement_With_Abort

   -------------------------------------------------------------------------------------------------
   --                                                                                             --
   -- Static evaluator and static properties of ranges and constraints                            --
   --                                                                                             --
   -------------------------------------------------------------------------------------------------

   type Biggest_Int is range System.Min_Int .. System.Max_Int; -- The best we can do
   Non_Static : constant Biggest_Int := -1;

   subtype Biggest_Natural          is Biggest_Int range 0          .. Biggest_Int'Last;
   subtype Extended_Biggest_Natural is Biggest_Int range Non_Static .. Biggest_Int'Last;

   type Extended_Biggest_Natural_List is array (Positive range <>) of Extended_Biggest_Natural;


   function Static_Expression_Value_Image (Expression : Asis.Expression) return Wide_String;
   --  Computes the value of Expression if it is a static expression
   --  and represents it as a (wide) string. For enumeration expressions, the
   --  image of the Pos value of the defining enumeration or character literal
   --  corresponding to the  value of the expression is returned.
   --
   --  For non-static expressions, or expressions that we cannot (yet) evaluate,
   --  an empty string is returned.
   --
   --  Currently implemented:
   --  All types:
   --     Constant
   --     Parenthesized expression
   --     'Pred, 'Succ, 'Pos, 'Val, 'First, 'Last
   --     Conversions and qualified expressions
   --  Integer: (provided values are within System.Min_Int .. System.Max_Int)
   --     Literal
   --     Named number
   --     + - * / **
   --  Real:
   --     Literal
   --     Named number
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


   function Discrete_Constraining_Bounds (Elem : Asis.Element) return Asis.Element_List;
   -- Elem must designate a type, a variable, a constant, a formal parameter,
   -- or a generic formal object.
   --
   -- Returns the expressions that constrain the values of a discrete type.
   -- Returned list has two elements
   -- Signed integer type : returns (First_Expression, Last_Expression)
   -- Subtype of modular  : returns (First_Expression, Last_Expression)
   -- Modular type        : returns (Nil_Element, Mod_Expression)
   -- Enumerated type     : returns (First_Defining_Name, Last_Defining_Name)
   --
   -- Returns the bounds that constrain the indexes of an array type.
   -- Returned list has an even number of elements (First(1), Last (1), First (2), Last (2), ...)
   -- Each pair of elements is the same as above
   --
   -- Returns Nil_Element_List if the type that applies to Elem is not discrete or array, or is formal
   --
   -- Appropriate Element_Kinds:
   --   An_Expression
   --   A_Declaration
   --   A_Definition
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
   --
   -- Returns Element_Kind:
   --   Not_An_Element
   --   An_Expression
   --   A_Defining_Name


   function Discrete_Constraining_Lengths (Elem : Asis.Element) return Extended_Biggest_Natural_List;
   -- Like Discrete_Constraining_Bounds, but returns the number of values in the range instead of
   -- the bounds if statically determinable
   -- Returns Non_Static (-1) if not statically determinable


   type Result_Confidence is (Unlikely, Possible, Certain);
   type Variable_Overlap  is (Complete, Partial, None);
   type Proximity is
      record
         Confidence : Result_Confidence;
         Overlap    : Variable_Overlap;
      end record;
   Same_Variable : constant Proximity := (Certain, Complete);

   function Variables_Proximity (Left, Right : Asis.Element) return Proximity;
   -- Determines if Left and Right can possibly refer to (par of) the same variables.
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
