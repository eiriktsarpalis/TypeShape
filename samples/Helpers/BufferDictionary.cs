using System.Buffers;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

namespace Helpers
{
    /// <summary>
    /// Read-only HashTable allowing lookup of ReadOnlySpan keys
    /// </summary>
	public sealed class BufferDictionary<TValue>
    {
        private readonly static bool s_is64BitProcess = Environment.Is64BitProcess;

        private readonly int[] _buckets;
        private readonly Entry[] _entries;
        private readonly ulong _fastModMultiplier;

        public BufferDictionary(IEnumerable<KeyValuePair<byte[], TValue>> input)
        {
            var source = input.ToArray();
            int size = source.Length;
            
            _buckets = new int[Math.Max(size, 1)];
            _entries = new Entry[size];

            if (s_is64BitProcess)
            {
                _fastModMultiplier = GetFastModMultiplier((uint)_buckets.Length);
            }

            int idx = 0;
            foreach (var kvp in source)
            {
                byte[] key = kvp.Key;
                uint hashCode = GetHashCode(kvp.Key);
                ref int bucket = ref GetBucket(hashCode);

                while (bucket != 0)
                {
                    ref Entry current = ref _entries[bucket - 1];
                    if (current.hashCode == hashCode && current.key.SequenceEqual(key))
                    {
                        throw new ArgumentException("duplicate key found");
                    }

                    bucket = ref current.next;
                }

                ref Entry entry = ref _entries[idx];
                entry.hashCode = hashCode;
                entry.key = key;
                entry.value = kvp.Value;
                bucket = ++idx;
            }
        }

        public int Count => _entries.Length;

        public bool TryGetValue(ReadOnlySpan<byte> key, [MaybeNullWhen(false)] out TValue? value)
        {
            Entry[] entries = _entries;
            uint hashCode = GetHashCode(key);
            int bucket = GetBucket(hashCode);

            while (bucket != 0)
            {
                ref Entry current = ref entries[bucket - 1];
                if (current.hashCode == hashCode && key.SequenceEqual(current.key))
                {
                    value = current.value;
                    return true;
                }

                bucket = current.next;
            }

            value = default;
            return false;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static uint GetHashCode(ReadOnlySpan<byte> key)
        {
            var hc = new HashCode();
            hc.AddBytes(key);
            return (uint)hc.ToHashCode();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ref int GetBucket(uint hashCode)
        {
            int[] buckets = _buckets;
            if (s_is64BitProcess)
            {
                return ref buckets[FastMod(hashCode, (uint)buckets.Length, _fastModMultiplier)];
            }
            else
            {
                return ref buckets[hashCode % (uint)buckets.Length];
            }
        }

        private struct Entry
        {
            public uint hashCode;
            public int next; // 1-based index of next entry in chain: 0 means end of chain
            public byte[] key;
            public TValue value;
        }

        /// <summary>Returns approximate reciprocal of the divisor: ceil(2**64 / divisor).</summary>
        /// <remarks>This should only be used on 64-bit.</remarks>
        private static ulong GetFastModMultiplier(uint divisor) =>
            ulong.MaxValue / divisor + 1;

        /// <summary>Performs a mod operation using the multiplier pre-computed with <see cref="GetFastModMultiplier"/>.</summary>
        /// <remarks>This should only be used on 64-bit.</remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static uint FastMod(uint value, uint divisor, ulong multiplier)
        {
            // We use modified Daniel Lemire's fastmod algorithm (https://github.com/dotnet/runtime/pull/406),
            // which allows to avoid the long multiplication if the divisor is less than 2**31.
            Debug.Assert(divisor <= int.MaxValue);

            // This is equivalent of (uint)Math.BigMul(multiplier * value, divisor, out _). This version
            // is faster than BigMul currently because we only need the high bits.
            uint highbits = (uint)(((((multiplier * value) >> 32) + 1) * divisor) >> 32);

            Debug.Assert(highbits == value % divisor);
            return highbits;
        }
    }
}