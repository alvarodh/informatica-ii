with Ada.Unchecked_Deallocation;

package body Hash_Maps_G is

   -- Liberar, borrar, punteros --
   procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_A);

   procedure Get (M       : in out Map;
                  Key     : in Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is

      P_Aux : Cell_A;

   begin
   
      Success := False;

      if M.Length /= 0 then
         -- Cogemos la posicion que nos interesa del array --
         P_Aux := M.H_Array(Hash(Key));
         
         loop
         
            exit when P_Aux = null or Success;
            
            if P_Aux.Key = Key then
               -- Lo hemos encontrado --
               Success := True;
               Value   := P_Aux.Value;
               exit;
            
            else
               -- Pasamos al siguiente --
               P_Aux := P_Aux.Next;
            
            end if;
         
         end loop;
      
      end if;
      
   end Get;

   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type) is

      P_Aux, P_Last : Cell_A;
      Found         : Boolean := False;

   begin
 
      if M.Length /= 0 then
      
         P_Aux := M.H_Array(Hash(Key));
         
         loop
         
            exit when P_Aux = null or Found;
            
            if P_Aux.Key = Key then
            
               Found       := True;
               P_Aux.Value := Value;
               exit;
            
            else
            
               P_Aux := P_Aux.Next;
            
            end if;
         
         end loop;
      
      end if;
      
      if not Found then
      
         if M.Length = Max then
            -- Se ha alcanzado el maximo --
            raise Full_Map;
         
         end if;
      
         M.Length := M.Length + 1;
      
         if M.H_Array(Hash(Key)) = null then
         
            M.H_Array(Hash(Key)) := new Cell'(Key, Value, null);
            
         else
         
            P_Last := M.H_Array(Hash(Key));
            
            loop
               -- Buscamos el ultimo --
               exit when P_Last.Next = null;
               
               P_Last := P_Last.Next;
            
            end loop;
            -- Añadimos el nuevo cliente al final --
            P_Aux := new Cell'(Key, Value, null);
            P_Last.Next := P_Aux;
            
         end if;
      
      end if;
      
   end Put;

   procedure Delete (M       : in out Map;
                     Key     : in Key_Type;
                     Success : out Boolean) is
   
      P_Aux, P_Ant : Cell_A;
      
   begin
   
      Success := False;
      P_Aux   := M.H_Array(Hash(Key));
      P_Ant   := null;
      
      if P_Aux /= null then
      
         loop
         
            if P_Aux.Key = Key then
               -- Hay que borrarlo --
               M.Length := M.Length - 1;
               Success  := True;
            
               if P_Ant /= null then
                  -- Esta por medio o al final --
                  P_Ant.Next := P_Aux.Next;
                  Free(P_Aux);
               
               else
                  -- Es el primero --
                  M.H_Array(Hash(Key)) := P_Aux.Next;
                  Free(P_Aux);
               
               end if;
               
               exit;
            
            else
               -- Continuar iterando --
               P_Ant := P_Aux;
               P_Aux := P_Aux.Next;
            
            end if;
         
            exit when P_Aux = null or Success;
         
         end loop;
      
      end if;
      
   end Delete;

   function Map_Length (M : Map) return Natural is
   
   begin
   
      return M.Length;
      
   end Map_Length;

   function First (M : Map) return Cursor is
   
      I     : Hash_Range := Hash_Range'Mod(0);
      Found : Boolean    := False;
      C     : Cursor;
      
   begin
   
      if M.Length /= 0 then
      
         loop
         
            if M.H_Array(I) /= null then
               
               Found       := True;
               C.Posicion  := I;
               C.Element_A := M.H_Array(I);
               C.Chain     := M.H_Array;
            
            else
            
               I := I + 1;
            
            end if;
            
            exit when Found;
         
         end loop;
      
      else
         -- Mapa vacío --
         C.Posicion  := I;
         C.Element_A := M.H_Array(I);
         C.Chain     := M.H_Array;
      
      end if;
      
      return C;
      
   end First;

   procedure Next (C : in out Cursor) is
   
   begin
   
      if C.Element_A.Next /= null then
         -- Si hay mas elementos en la lista --
         C.Element_A := C.Element_A.Next;
         
      else
      
         if C.Posicion < Hash_Range'Last then
            -- Si no es el ultimo --
            loop
            
               C.Posicion  := C.Posicion + 1;
               C.Element_A := C.Chain (C.Posicion);
               -- Buscamos una celda que no este vacía --
               exit when C.Element_A /= null or C.Posicion = Hash_Range'Last;
               
            end loop;
         
         else
         
            raise No_Element;
         
         end if;
         
      end if;
      
   end Next;

   function Has_Element (C : Cursor) return Boolean is
   
   begin
   
      return C.Element_A /= null;
      
   end Has_Element;

   function Element (C : Cursor) return Element_Type is
   
      E : Element_Type;
   
   begin
   
      if C.Element_A /= null then
      
         E.Key   := C.Element_A.Key;
         E.Value := C.Element_A.Value;
         
         return E;
         
      else
      
         raise No_Element;
         
      end if;
      
   end Element;

end Hash_Maps_G;
