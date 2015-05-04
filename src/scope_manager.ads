----------------------------------------------------------------------
--  Scope_Manager - Package specification                           --
--  Copyright (C) 2004-2015 Adalog                                  --
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

-- Asis
with
  Asis;

package Scope_Manager is
   --  This package provides facilities for applications that need to manage
   --  information associated to scopes. A scope is a construct that can
   --  contain declarations.
   --
   --  The scope level is the nesting depth of the scope.
   --  The scope level is incremented when the scope is entered, i.e.
   --  while traversing the construct itself, the current scope is the one of the
   --  place where the construct is declared, but when traversing anything inside it
   --  the current scope is one more.
   --  The current scope of a root library unit is 0, and therefore the scope of anything
   --  inside a root library unit is 1.
   --  Note that a "unit" scope is not necessarily at level 1, in the case of subunits.
   --
   --  Proper bodies have the same depth as their corresponding stub.
   --
   --  Note that when processing context clauses, the current scope is the one
   --  of the following library unit. This is what the user would expect,
   --  although from an ASIS point of view, the associated construct has not
   --  yet been entered.

   --  Usage
   --  ------
   --
   -- This package must know when certain constructs are being traversed; since in an ASIS application
   -- the traversal is in the realm of the user application, the package offers some services ("plugs")
   -- that must be called at appropriate places during the traversal.
   -- The plugs are at the end of the specification, in the "plugs" section; appropriate comments describe
   -- under what circumstances each plug must be called.
   --
   -- This unit relies on the fact that a parent unit is always processed before its children.
   -- In AdaControl, this is ensured by the options' analyzer. See Adactl_Options.Add_Unit.
   -- Make sure the same holds in your application
   --


   -----------------------------------------------------------------------------------
   -- Scopes
   -----------------------------------------------------------------------------------
   Max_Scopes : constant := 50;
   -- Maximum depth of scopes nesting.

   type Scope_Range is range 0 .. Max_Scopes;
   Compilation_Unit_Scope : constant Scope_Range := 1;
   type Scope_List is array (Scope_Range range <>) of Asis.Element;

   type Declaration_Origin is (Same_Unit, Specification, Parent);
   -- Declaration_Origin is Specification if the current scope is a body and the info
   -- comes from the corresponding specification.
   -- Declaration_Origin is Parent if the current scope is a child unit and the info
   -- comes from some parent.

   function Is_Scope (Element : Asis.Element) return Boolean;
   -- True if Element is something considered a scope from the point of view of
   -- the scope manager

   function Current_Depth   return Scope_Range;
   function Current_Scope   return Asis.Element;
   function Enclosing_Scope return Asis.Element;
   function Active_Scopes   return Scope_List;
   function Is_Active (Scope : Asis.Element) return Boolean;
   -- True iff Scope is one of the elements of Active_Scopes

   function In_Private_Part (Scope : Scope_Range := Current_Depth) return Boolean;
   function In_Context_Clauses return Boolean;

   function Is_Current_Scope_Global   return Boolean;
   function Is_Enclosing_Scope_Global return Boolean;
   -- A scope is global if itself and all enclosing scopes are all
   -- packages or generic packages


   procedure Reset (Deactivate : Boolean);
   -- Cleans up all active scope and all Scoped_Store data
   -- To be used at the end of a full processing (like a Go command in AdaControl), or in the case
   -- of a premature termination due to an unexpected exception.
   -- If Deactivate is True, scoped stores are also deactivated, which should not
   -- be done while recovering from an error.

   -----------------------------------------------------------------------------------
   -- Scoped_Store                                                                  --
   -- Management of user data associated to scopes                                  --
   -----------------------------------------------------------------------------------

   type Iterator_Mode is (All_Scopes, Unit_Scopes, Current_Scope_Only);

   type Scoping_Procedure is access procedure (Scope : Asis.Element);
   -- This declaration is for use in the private part of Scoped_Store,
   -- no use for the users of this package. (No harm either).

   generic
      type Data (<>) is private;
      with function Equivalent_Keys (L, R : Data) return Boolean is "=";
      with procedure Clear (Item : in out Data) is null;
   package Scoped_Store is
      -- This package manages user data that are to be associated to a scope.
      -- It is managed as a stack. Data associated to a scope are automatically
      -- deleted when the scope is exited.
      -- Data pushed when processing a package spec, a generic package spec, a
      -- task spec or a protected spec is temporarily removed at the end of the spec,
      -- and restored at the beginning of the corresponding body. It is deleted at the
      -- end of the body, unless it is a compilation unit.
      --
      -- The Clear formal procedure is called whenever this package automatically
      -- deallocates stack elements; if Data contains dynamically allocated storage,
      -- it must provide a non-null Clear procedure to free this storage, or memory
      -- leaks will result
      --
      -- The package is deactivated after each run, in order to avoid managing scopes for
      -- rules that are not active.

      procedure Activate;
      -- Must be called before *each* run (typically from a Prepare procedure)

      procedure Push (Info : in Data);
      -- Adds Info on top of stack, associated to current scope

      procedure Push_Enclosing (Info : in Data);
      -- Adds Info associated to the enclosing scope of the current scope
      -- If Current_Scope is a library unit, the info is associated to the scope level 0,
      -- and the corresponding Current_Data_Scope returns Nil_Element


      --
      -- Iterator
      --

      -- Iterates through stored data.
      -- Data are returned by Current_Data from top to bottom but not removed
      -- from the stack.

      -- It is possible to add new data with Push while iterating; since they are added
      -- on top (i.e. above the current position of the iterator), it does not
      -- change the behaviour of the iterator.
      -- The same does not hold when adding data with Push_Enclosing, which should therefore
      -- not be used while iterating.

      function  Data_Available     return Boolean;
      -- False when iterator is exhausted (or empty)
      function  Current_Data       return Data;
      function  Current_Data_Level return Scope_Range;
      function  Current_Data_Scope return Asis.Element;
      function  Current_Origin     return Declaration_Origin;
      procedure Update_Current (Info : in Data);

      procedure Reset (Mode : Iterator_Mode);
      -- Sets the iterator to the top of the stack.
      -- If the mode of Reset is Current_Scope_Only, only data associated to the current scope
      -- are returned by the iterator.
      -- If the mode of Reset is Unit_Scopes, only data associated to the scope of the current
      -- compilation unit and above are returned.
      -- If the mode is All_Scopes, all data are returned.

      procedure Reset (Info : Data; Mode : Iterator_Mode);
      -- Initializes the iterator on data with Equivalent_Keys to the provided Info
      -- (Data_Available returns False if not found).
      -- If the mode of Reset is Current_Scope_Only, only data associated to the scope
      -- of Info are returned by the iterator.
      -- If the mode of Reset is Unit_Scopes, only data associated to the scope
      -- of the compilation unit of Info and above are returned by the iterator.
      -- If the mode is All_Scopes, all data from Info to the bottom of the stack are returned.

      procedure Continue (Mode : Iterator_Mode);
      -- Continue is like the second form of Reset, starting from the current position.

      procedure Next;
      -- Moves to the next element. If the iterator is exhausted (i.e.
      -- Data_Available is False), raises Constraint_Error.

      procedure Delete_Current;
      -- Removes current data from the store
      -- After a call to Delete_Current, the iterator moves to the next position.

   private
      -- The following declarations are here because they are not allowed
      -- in a generic body.

      procedure Enter_Unit    (Scope : Asis.Element);
      procedure Enter_Scope   (Scope : Asis.Element);
      procedure Enter_Private (Scope : Asis.Element);
      procedure Exit_Scope    (Scope : Asis.Element);
      procedure Clear_All     (Scope : Asis.Element);
      -- The parameters of Enter_Private and Clear_All are not used,
      -- they are here just to match the profile.

      Unit_Access    : constant Scoping_Procedure := Enter_Unit'Access;
      Scope_Access   : constant Scoping_Procedure := Enter_Scope'Access;
      Private_Access : constant Scoping_Procedure := Enter_Private'Access;
      Exit_Access    : constant Scoping_Procedure := Exit_Scope'Access;
      Clear_Access   : constant Scoping_Procedure := Clear_All'Access;
   end Scoped_Store;


   ----------------------------------------------------------------------------
   --                                                                        --
   --  Plugs section                                                         --
   --                                                                        --
   ----------------------------------------------------------------------------

   --
   -- To be called at Unit level:
   --
   procedure Enter_Unit  (Unit  : in Asis.Compilation_Unit);
   -- To be called at the beginning of processing a compilation unit, before any traversal

   procedure Exit_Unit   (Unit  : in Asis.Compilation_Unit);
   -- To be called at the end of processing a compilation unit, after all traversal

   procedure Exit_Context_Clauses;
   -- To be called after traversing context clauses, before traversing the attached unit

   --
   -- To be called at Element level:
   --
   procedure Enter_Scope (Scope : in Asis.Element; Is_Unit : Boolean := False);
   -- To be called each time a scope is entered (see what it means in the body of Is_Scope),
   -- after processing the Scope globally (i.e., after calling Enter_Scope, you are inside the entity)
   -- Use the default value of Is_Unit (Is_Unit is True only for internal calls)

   procedure Enter_Private_Part;
   -- To be called between the traversal of the visible part of an entity with a private part
   -- (package, generic package, single task, task type, single protected, protected type)
   -- and the traversal of its private part.

   procedure Exit_Scope  (Scope : in Asis.Element; Force : Boolean := False);
   -- To be called each time a scope is left (see what it means in the body of Is_Scope)
   -- Use the default value of Force (Force is True only for internal calls)

end Scope_Manager;
