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

        public ListEntry(Region region, T element)
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
        private Region _region;
        private ListEntry<T> _head;

        public RegionList(Region region)
        {
            _region = region;
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
            var listEntry = new ListEntry<T>(_region, item);
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

        public Item(Region region, int itemId, int category)
        {
            this.ItemId = itemId;
            this.Category = category;
        }

        public Item Clone(Region region)
        {
            var newItem = new Item(region, this.ItemId, this.Category);
            return newItem;
        }
    };

    public class ItemList
    {
        private RegionList<Item> _items;    
        public RegionList<Item> Items 
        {
            get { return _items; }
        }

        private Region _region;
        public Region Region 
        {
            get { return _region; }
        }

        public ItemList(Region region)
        {
            _region = region;
            _items = new RegionList<Item>(region);
            for(int i = 0; i < 2000000/64; i++)
            {
                _items.Add(new Item(region, 0, 0));
            }        

        }
    }


    public class ListActor
    {
        private static int BUFFER_SIZE = 100;

        private RegionList<ItemList> _list;
        private int _recvCount;

        public ListActor(Region parentRegion)
        {
            var actorRegion = RegionAllocator.AllocateRegion(parentRegion, 100000);
            _list = new RegionList<ItemList>(actorRegion);
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
                        var itemList = _list.RemoveAtAndGet(0);                
                        RegionAllocator.FreeRegion(itemList.Region);
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
            var parentRegion = RegionAllocator.AllocateRegion(320000);   
            var actor = new ListActor(parentRegion);
            Console.WriteLine("Allocated parent region");
            for(int count = 0; count < 10000; count++)
            {
                var messageRegion = RegionAllocator.AllocateRegion(parentRegion, 2000000);            
                var itemList = new ItemList(messageRegion);
                actor.OnRecv(itemList);               
            }
        }   
    }    
}
