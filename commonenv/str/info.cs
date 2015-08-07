// --------------------------------------------------------------------
// 
// Copyright (c) Microsoft Corporation.  All rights reserved.
// 
// --------------------------------------------------------------------
using System;
using System.Collections.Generic;
using Bartok.PlatformAbstraction;
using Platform;
using Platform.Collections;
using Platform.Collections.Lists;
using Platform.Collections.Maps;
using Platform.Collections.Sets;
using Platform.Unicode;

namespace Bartok.Utility
{
    /// <summary>
    /// zapp
    /// </summary>
    public class [[type]] : InfoTypeKey
    {
        zapp;

        // Constructor for use when decoding keys.
        public [[type]](InfoKeyKind kind)
            : base(kind)
        {
        }

        [[type]](InfoKeyKind kind, AMutableList<InfoTypeKey> typeArguments, 
                         )
            : base(kind, typeArguments)
        {
       }

        /// <summary>
        /// Return a MemberKey with the given parameters, making a new one if necessary, but using
        /// an old interned one if possible.
        /// </summary>
        public static [[type]] Intern(InfoKeyKind kind, params InfoTypeKey[] typeArgs)
        {
            AMutableList<InfoTypeKey> typeArguments = null;
            if ((typeArgs != null) && (typeArgs.Length > 0)) {
                typeArguments = new MutableList<InfoTypeKey>(typeArgs.Length);
                foreach (var arg in typeArgs) {
                    typeArguments.Add(arg);
                }
            }
            [[type]] newKey = new [[type]](kind, typeArguments);
            return ([[type]])InfoKey.Intern(newKey);
        }

        public override void Encode(ByteArrayProcessor buffer)
        {
            base.Encode(buffer);
        }

        public static new [[type]] DecodeId(ByteArrayProcessor buffer)
        {
            return ([[type]]) InfoKey.DecodeId(buffer);
        }

        public override void FinishDecode(ByteArrayProcessor buffer)
        {
            base.FinishDecode(buffer);
        }

        public override void PrettyPrint(PlatformTextWriter tw, string format, params object[] args)
        {
            if (s_verbose) {
                this.PrettyBegin(tw, "[[type]]", format, args);
                this.PrettyEnd(tw);
            }
            else {
            }
        }

        string PreArgumentString()
        {
            MutableString sb = new MutableString("");
            return sb.ToString();
        }

        public override string ComputeInfoName(bool makeInternal)
        {
            MutableString sb = new MutableString("");
            sb.Append(this.PreArgumentString());
            return sb.ToString();
        }

        public override void VisitInfo(Func<InfoBase, bool> preAction, Action<InfoBase, bool> postAction)
        {
            bool preResult = preAction(this);
            if (preResult) {
                base.VisitInfo(preAction, postAction);
            }
            postAction(this, preResult);
        }

        public override int Compare(InfoKey b, bool makeInternal)
        {
            int baseCompare = base.Compare(b, makeInternal);
            if (baseCompare != 0) {
                return baseCompare;
            }
            [[type]] other = ([[type]])b;
            return 0;
        }

        public override int GetHashCode()
        {
            int resultHash = this.m_hashcode;
            if (resultHash == 0) {
                resultHash = base.GetHashCode();
                // resultHash = Platform.HashUtilities.CombineHashCodes(resultHash);
                if (resultHash == 0) {
                    resultHash = 1;
                }
                this.m_hashcode = resultHash;
            }
            return resultHash;
        }

        public new struct ExternalComparator : IOrderedComparator<[[type]]>
        {
            public static int Compare([[type]] a, [[type]] b)
            {
                return a.Compare(b, false);
            }

            int IOrderedComparator<[[type]]>.Compare([[type]] a, [[type]] b)
            {
                return a.Compare(b, false);
            }

            public static bool Equals([[type]] a, [[type]] b)
            {
                return [[type]].ExternalComparator.Compare(a, b) == 0;
            }

            bool IEqualityComparator<[[type]]>.Equals([[type]] a, [[type]] b)
            {
                return [[type]].ExternalComparator.Compare(a, b) == 0;
            }

            public static int GetHashCode([[type]] a)
            {
                return a.GetHashCode();
            }

            int IEqualityComparator<[[type]]>.GetHashCode([[type]] a)
            {
                return a.GetHashCode();
            }
        }

        public new struct InternalComparator : IOrderedComparator<[[type]]>
        {
            public static int Compare([[type]] a, [[type]] b)
            {
                return a.Compare(b, true);
            }

            int IOrderedComparator<[[type]]>.Compare([[type]] a, [[type]] b)
            {
                return a.Compare(b, true);
            }

            public static bool Equals([[type]] a, [[type]] b)
            {
                return [[type]].InternalComparator.Compare(a, b) == 0;
            }

            bool IEqualityComparator<[[type]]>.Equals([[type]] a, [[type]] b)
            {
                return [[type]].InternalComparator.Compare(a, b) == 0;
            }

            public static int GetHashCode([[type]] a)
            {
                return a.GetHashCode();
            }

            int IEqualityComparator<[[type]]>.GetHashCode([[type]] a)
            {
                return a.GetHashCode();
            }
        }
    }
}
