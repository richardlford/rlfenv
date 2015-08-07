            public override void PrettyPrint(PlatformTextWriter tw, string format, params object[] args)
            {
                this.PrettyBegin(tw, "[[type]]", format, args);
                this.PrettyEnd(tw);
            }
