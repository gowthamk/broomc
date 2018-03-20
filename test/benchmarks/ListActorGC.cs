using System;
using System.Collections.Generic;
using Midori.Runtime;

namespace Broom 
{
    public class ListEntry<T>
    {
        private T _element;
        private ListEntry<T> _tail;

        public T Element
        {
            get { return _element; }
        }

        public ListEntry<T> Tail 
        { 
            get { return _tail; } 
        }

        public ListEntry(T element)
        {
            _element = element;
            _tail = null;
        }

        public void Extend(ListEntry<T> tail)
        {
            _tail = tail;
        }
    }

    public class RegionList<T> 
    {
        private int _count;
        private ListEntry<T> _head;

        public RegionList()
        {
            _count = 0;
        }

        public int IndexOf(T item)
        {
            throw new NotImplementedException();
        }

        public void Insert(int index, T item)
        {
            throw new NotImplementedException();
        }

        public T RemoveAtAndGet(int index)
        {
            if (index > this.Count)
            {
                throw new InvalidOperationException("Insufficient elements");
            }

            ListEntry<T> current = _head;
            if (index == 0)
            {
                _head = _head.Tail;
                _count--;
                return current.Element;                
            }

            ListEntry<T> previous = null;
            for (int count = 0; count < index; count++)
            {
                previous = current;
                current = current.Tail;
            }

            var deleted = current;
            previous.Extend(current.Tail);
            _count--;
            return deleted.Element;

        }

        public void RemoveAt(int index)
        {
            if (index > this.Count)
            {
                throw new InvalidOperationException("Insufficient elements");
            }

            ListEntry<T> current = _head;
            if (index == 0)
            {
                _head = _head.Tail;
                _count--;
            }

            ListEntry<T> previous = null;
            for (int count = 0; count < index; count++)
            {
                previous = current;
                current = current.Tail;
            }

            previous.Extend(current.Tail);
            _count--;
        }

        public T this[int index]
        {
            get
            {
                throw new NotImplementedException();
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        public void Add(T item)
        {
            var listEntry = new ListEntry<T>(item);
            listEntry.Extend(_head);
            _head = listEntry;
            _count++;
        }

        public void Clear()
        {
            throw new NotImplementedException();
        }

        public bool Contains(T item)
        {
            throw new NotImplementedException();
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            throw new NotImplementedException();
        }

        public int Count
        {
            get { return _count; }
        }

        public bool IsReadOnly
        {
            get { throw new NotImplementedException(); }
        }

        public bool Remove(T item)
        {
            throw new NotImplementedException();
        }

        public IEnumerator<T> GetEnumerator()
        {
            throw new NotImplementedException();
        }

        /*
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            throw new NotImplementedException();
        }
        */
    }

    public class Item
    {
        public int ItemId;
        public int Category;

        public Item(int itemId, int category)
        {
            this.ItemId = itemId;
            this.Category = category;
        }
    };

    public class ItemList
    {
        private RegionList<Item> _items;    
        public RegionList<Item> Items 
        {
            get { return _items; }
        }

        public ItemList()
        {
            _items = new RegionList<Item>();
            for(int i = 0; i < 2000000/64; i++)
            {
                _items.Add(new Item(0, 0));
            }        

        }
    }


    public class ListActor
    {
        private static int BUFFER_SIZE = 100;

        private RegionList<ItemList> _list;
        private int _recvCount;

        public ListActor()
        {
            _list = new RegionList<ItemList>();
            _recvCount = 0;
        }

        public void OnRecv(ItemList list)
        { 
            _recvCount++;
            Console.WriteLine("Receiving message {0}", _recvCount);
            _list.Add(list);

            if (_recvCount % BUFFER_SIZE == 0)
            {
                Console.WriteLine("Freeing regions");                    
                try
                {
                    for(int count = 0; count < BUFFER_SIZE; count++)
                    {
                        _list.RemoveAt(0);                
                    }
                }
                catch (Exception)
                {
                }
            }
        }
    }

    public class Program
    {
        public static void Main(string[] args)
        {       
            var actor = new ListActor();
            for(int count = 0; count < 10000; count++)
            {
                var itemList = new ItemList();
                actor.OnRecv(itemList);               
            }
        }   
    }    
}
