#!/usr/bin/env python

from argparse import ArgumentParser
from jupyter_client import KernelManager, find_connection_file
from base64 import decodestring
import os
import re
import sys
from time import sleep
from traceback import format_exc
from Queue import Empty


def parse_args(args):
    parser = ArgumentParser(description='IPython markdown + LaTeX engine for knitr')

    parser.add_argument('code',
                        type=str,
                        nargs=1,
                        help='Python code')

    parser.add_argument('-i', '--inspect',
                        dest='inspect',
                        action='store_true',
                        help='Inspect code')

    parser.add_argument('-c', '--complete',
                        dest='complete',
                        action='store_true',
                        help='Complete code')

    parser.add_argument('--cursor',
                        type=int,
                        dest='cursor',
                        default=None,
                        help='Cursor position for completion or inspection')

    parser.add_argument('--level',
                        type=int,
                        dest='level',
                        default=0,
                        help='Documentation level')

    parser.add_argument('-p', '--prefix',
                        type=str,
                        dest='prefix',
                        default='figure/image',
                        help='Image prefix')

    parser.add_argument('-s', '--figshow',
                        type=str,
                        dest='figshow',
                        default='asis',
                        choices=['asis', 'hold', 'hide'],
                        help='How to display plots')

    parser.add_argument('--width',
                        type=float,
                        dest='width',
                        default=None,
                        help='Plot width (inches)')

    parser.add_argument('--height',
                        type=float,
                        dest='height',
                        default=None,
                        help='Plot height (inches)')

    parser.add_argument('--dpi',
                        type=float,
                        dest='dpi',
                        default=None,
                        help='Plot resolution (dots per inch)')

    parser.add_argument('--outwidth',
                        dest='outwidth',
                        default='\\linewidth',
                        help='Latex plot width')

    parser.add_argument('-t', '--to',
                        type=str,
                        dest='to',
                        default='markdown',
                        choices=['markdown', 'nohtml', 'latex', 'asis'],
                        help='Output format')

    parser.add_argument('--imageformat',
                        type=str,
                        dest='imageformat',
                        default=None,
                        choices=['png', 'pdf'],
                        help='Image format. Defaults to pdf for latex output, and png otherwise')

    parser.add_argument('-m', '--message',
                        type=bool,
                        dest='message',
                        default=True,
                        help='Display printed messages')

    parser.add_argument('-k', '--kernel',
                        type=str,
                        dest='kernel',
                        default='existing',
                        help='Optional name of an ipython kernel, e.g. kernel-16284.json')

    parser.add_argument('-d', '--debug',
                        dest='debug',
                        action='store_true',
                        help='Display debug information')

    config = parser.parse_args(args)
    if config.imageformat is None:
        config.imageformat = 'pdf' if config.to == 'latex' else 'png'

    return config

def complete(kc, code, cursor):
    msg_id = kc.complete(code, cursor_pos=cursor)
    # print "complete msg_id=%s" % msg_id

    while True:
        m = kc.get_shell_msg()
        # print "parent_id=%s type=%s content.keys=%s content=%s" % (m['parent_header']['msg_id'], m['msg_type'], m['content'].keys(), str(m['content'])[:80]

        if m['parent_header'].get('msg_id') != msg_id:
            continue

        r = m.get('content')
        if r is not None:
            r = r.get('matches')

        return r

def inspect(kc, code, cursor, level):
    msg_id = kc.inspect(code, cursor_pos=cursor, detail_level=level)
    # print "complete msg_id=%s" % msg_id

    while True:
        m = kc.get_shell_msg()
        # print "parent_id=%s type=%s content.keys=%s content=%s" % (m['parent_header']['msg_id'], m['msg_type'], m['content'].keys(), str(m['content'])[:80]

        if m['parent_header'].get('msg_id') != msg_id:
            continue

        r = m.get('content')
        if r is not None:
            r = r.get('data')
        if r is not None:
            r = r.get('text/plain')

        return r

def execute(kc, code):
    msg_id = kc.execute(code)
    # print "exec msg_id=%s" % msg_id
    result = []

    while True:
        m = kc.get_iopub_msg()
        # print "parent_id=%s type=%s content.keys=%s content=%s" % (m['parent_header']['msg_id'], m['msg_type'], m['content'].keys(), str(m['content'])[:80]

        if m['parent_header'].get('msg_id') != msg_id:
            continue

        if m['msg_type'] == 'status':
            if m['content']['execution_state'] == 'idle':
                return result
            continue

        result.append((m['msg_type'], m['content']))


def format_text(text, to='markdown', html=None):
    if to == 'asis':
        return text
    if to == 'latex':
        return '\\begin{verbatim}\n' + text + '\n\\end{verbatim}'
    if to == "nohtml" or html is None:
        return '```\n' + text + '\n```'
    return '<div class="output">\n' + html + '\n</div>'

def format_display(html, to='markdown'):
    if to == 'markdown':
        return '<div class="display">\n' + html + '\n</div>'
    if to == 'asis':
        return html
    if to == "nohtml":
        return ''
    raise TypeError("html display code is not supported in output format '%s'" % to)


def format_javascript(code, to='markdown'):
    if to == 'markdown':
        return '<script type="text/javascript">\n' + code + '\n</script>'
    raise TypeError("javascript code is not supported in output format '%s'" % to)


def format_image(filename, text='', config=None):
    if config.to == 'latex':
        return '\includegraphics[width=%s]{%s}' % (config.outwidth, os.path.splitext(filename)[0])
    return '![%s](%s)' % (text, filename)


def format_result(r, config):
    result = []

    image_counter = 1
    images_in_hold = []
    error = False

    for msg_type, content in r:
        if config.debug:
            print '[DEBUG] msg_type=%s content.keys=%s content.data.keys=%s, content=%s' % (
                msg_type, content.keys(), content.get('data', {}).keys(), str(content)[:1024])

        if msg_type == 'stream' and config.message and \
                re.match('<ggplot.*>', content['text']) is None:
            try:
                result.append(format_text(content['text'][:-1], config.to))
            except Exception as e:
                result.append(
                    format_text('[ERROR] ipython_exec was not able to display the streamed message\n' \
                                + format_exc(), config.to))
                error = True

        if msg_type == 'error':
            try:
                error = '[ERROR] {}: {}'.format(content['ename'], content['evalue'])
                result.append(format_text(error, config.to))
                error = True
            except Exception as e:
                result.append(format_text('[ERROR] ipython_exec was not able to parse the error\n' \
                                          + format_exc(), config.to))
                error = True

        if msg_type == 'execute_result':
            try:
                data = content['data']
                plain = data.get('text/plain')
                html = data.get('text/html')

                if re.match('(\\[|)<matplotlib.* at 0x.*>(\\]|)', plain) is None \
                    and re.match('<ggplot.*>', plain) is None:
                    result.append(format_text(plain, config.to, html))
            except Exception as e:
                result.append(
                    format_text('[ERROR] ipython_exec was not able to parse the execution result\n' \
                                + format_exc(), config.to))
                error = True

        if msg_type == 'display_data':
            try:
                data = content['data']

                image_data = data.get('image/png')
                if image_data:
                    image_ext='png'
                else:
                    image_data = data.get('application/pdf')
                    if image_data:
                        image_ext='pdf'

                if image_data and config.figshow != 'hide':
                    filename = '%s_%d.%s' % (config.prefix, image_counter, image_ext)
                    image_counter += 1
                    if not os.path.exists(os.path.dirname(filename)):
                        os.makedirs(os.path.dirname(filename))
                    with open(filename, "w") as f:
                        f.write(decodestring(image_data))

                    text = data.get('text/plain', '')
                    image = format_image(filename, text, config)
                    if config.figshow == 'hold':
                        images_in_hold.append(image)
                    else:
                        result.append(image)

                js = data.get('application/javascript')
                if js:
                    result.append(format_javascript(js))

                html = data.get('text/html')
                if html:
                    result.append(format_display(html, config.to))

            except Exception as e:
                result.append(format_text('[ERROR] ipython_exec was not able to parse the display data\n' \
                                          + format_exc(), config.to))
                error = True

    for md in images_in_hold:
        result.append(md)

    return error, '\n'.join(result)


def kernel_client(config):
    try:
        if config.kernel != 'existing':
            cnx_file = find_connection_file(filename=config.kernel)
        else:
            cnx_file = find_connection_file()

        km = KernelManager(connection_file=cnx_file)
        km.load_connection_file()

        kc = km.client()
        kc.start_channels()

        try:
            msg = kc.shell_channel.get_msg()
        except Empty:
            try:
                # try again :)
                sleep(1)
                msg = kc.shell_channel.get_msg()
            except Empty:
                raise RuntimeError('No message found in the channel. Is the kernel alive?')

    except Exception as e:
        print format_text('[ERROR] ipython_exec was not able to connect to the desired jupyter kernel\n' + \
                          "[HINT] Execute 'ipython_start_kernel' or 'jupyter console' first\n" + \
                          format_exc(), config.to)
        exit(1)

    return kc


if __name__ == '__main__':
    config = parse_args(sys.argv[1:])

    kc = kernel_client(config)

    if config.inspect:
        r = inspect(kc, config.code[0], config.cursor, config.level)
        if r:
            print r
        exit()

    if config.complete:
        import json
        r = complete(kc, config.code[0], config.cursor)
        print json.dumps(r)
        exit()

    # figure dimensions
    if config.width or config.height or config.dpi or config.imageformat:
        mploptions = []
        mploptions.append('import matplotlib')
        if config.dpi:
            mploptions.append("matplotlib.rcParams['figure.dpi'] = %d" % config.dpi)
        if config.width or config.height:
            mploptions.append("matplotlib.rcParams['figure.figsize'] = (%f,%f)" % (
            config.width, config.height))
        if config.imageformat:
            mploptions.append("""from IPython.display import set_matplotlib_formats
set_matplotlib_formats('%s')""" % config.imageformat)
        r = execute(kc, '\n'.join(mploptions))
        err, result = format_result(r, config)
        if err:
            if result != "":
                print result
            exit(err)

    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    if result != "":
        print result

    exit(err)
