using System;
using System.Collections.Generic;
using Midori.Runtime;

namespace TestActors
{
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
        private RegionList<Item> _list;
        public RegionList<Item> List { get { return _list; } } 

        public ItemList(Region region, RegionList<Item> list)
        {
            _list = list;
        }
    }

	class Program
	{
		public static void Main(string[] args)
		{
            var actor = new ListActor();
            for(int count = 0; count < 1000; count++)
            {
                var messageRegion = RegionAllocator.AllocateRegion(2000000);
                var oneMsgList = new RegionList<Item>(messageRegion);
                for (int j = 0; j < 2000000 / 32; j++)
                {
                    var item = new Item(messageRegion, 1, 1);
                    oneMsgList.Add(item);
                }

                actor.OnRecv(messageRegion, oneMsgList);               
            }
		}
	}

    public class ListActor
    {
        private Region _actorRegion;

        private RegionList<ItemList> _list;
        private int _recvCount;

        public ListActor()
        {
            _actorRegion = RegionAllocator.AllocateRegion(64000);
            _list = new RegionList<ItemList>(_actorRegion);
        }

        public void OnRecv(Region messageRegion, RegionList<Item> list)
        {
            var newItemList = new ItemList(_actorRegion, list);
            _list.Add(newItemList);
            _recvCount++;

            if (_recvCount % 5 == 0)
            {
                try
                {
                    var itemList = _list.RemoveAtAndGet(_list.Count - 3);
                    RegionAllocator.FreeRegion(itemList.List.region);
                }
                catch (Exception)
                {
                }
            }
        }
    }

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

        public Region region { get { return _region; } }

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
}